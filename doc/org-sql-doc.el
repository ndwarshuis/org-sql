;;; org-sql-doc.el --- Build documentation for org-sql -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Nathan Dwarshuis

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; These are functions to build the ERDs and table descriptions

;;; Code:

(require 'package)
(require 'dash)
(require 's)
(require 'f)
(require 'org-sql)

(defconst org-sql-doc-dir "doc"
  "The location of the doc files")

;;; entity relationship diagrams

(defun org-sql-er-format-column (pks fks config column)
  (-let* (((name . (&plist :type :constraints)) column)
          (type* (if (eq type 'enum)
                     (org-sql--case-mode config
                       ((mysql pgsql) "ENUM")
                       ((sqlite sqlserver)
                        (org-sql--format-create-tables-type config name column)))
                   (org-sql--format-create-tables-type config name column)))
          (constraints* (-some->> constraints
                          (org-sql--format-column-constraints)))
          (name* (--> (org-sql--format-column-name name)
                   (if (memq name pks) (format "*%s" it) it)
                   (if (memq name fks) (format "+%s" it) it))))
    (format "%s {label: \"%s\"}" name* (s-join ", " (-non-nil (list type* constraints*))))))

(defun org-sql-er-format-table (config table)
  (-let* (((name . (&alist 'columns 'constraints)) table)
          (pks (plist-get (alist-get 'primary constraints) :keys))
          (fks (->> (--filter (eq (car it) 'foreign) constraints)
                    (--mapcat (plist-get (cdr it) :keys))
                    (-uniq)))
          (columns* (->> columns
                         (--map (org-sql-er-format-column pks fks config it))
                         (s-join "\n"))))
    (format "[%s]\n%s" name columns*)))

(defun org-sql-er-get-tables (config)
  (->> org-sql--table-alist
       (--map (org-sql-er-format-table config it))
       (s-join "\n\n")))

(defun org-sql-er-format-table-relationships (table)
  (-let* (((name . (&alist 'constraints)) table)
          (fks (--filter (eq 'foreign (car it)) constraints)))
    (--map (-let* (((&plist :ref :cardinality :keys) (cdr it))
                   (rel (pcase cardinality
                          (`nil (error "Need cardinality for %s" name))
                          (`one-to-one "1--1")
                          (`one-or-none-to-one "?--1")
                          (`many-to-one "+--1")
                          (`many-or-none-to-one "*--1")
                          (e (error "Unknown cardinality: %s" e))))
                   (parent (plist-get (cdr it) :ref)))
             (if (< 1 (length fks))
                 (->> (-map #'org-sql--format-column-name keys)
                      (s-join ", ")
                      (format "%s %s %s {label: \"%s\"}" name rel parent))
               (format "%s %s %s" name rel parent)))
           fks)))

(defun org-sql-er-get-relationships ()
  (->> org-sql--table-alist
       (-mapcat #'org-sql-er-format-table-relationships)
       (-non-nil)
       (s-join "\n")))

(defun org-sql-format-er-file (config)
  (let* ((db-name (org-sql--case-mode config
                    (mysql "MySQL/MariaDB")
                    (pgsql "PostgreSQL")
                    (sqlite "SQLite")
                    (sqlserver "SQL Server")))
         (title (format "title {label: \"Org-SQL ERD (%s)\"}" db-name))
         (tables (org-sql-er-get-tables config))
         (relationships (org-sql-er-get-relationships)))
    (s-join "\n\n" (list title tables relationships))))

(defun org-sql-write-erd (config)
  (if (not (executable-find "erd"))
      (error "Install erd to complete documentation")
    (let ((er (org-sql-format-er-file config))
          (inpath (f-join (temporary-file-directory) "org-sql-erd.er"))
          (outpath (->> (org-sql--case-mode config
                          (mysql "mysql")
                          (pgsql "postgres")
                          (sqlite "sqlite")
                          (sqlserver "sqlserver"))
                        (format "erd-%s.pdf")
                        (f-join org-sql-doc-dir "erd"))))
      (f-write-text er 'utf-8 inpath)
      (call-process "erd" nil nil nil "-i" inpath "-o" outpath)
      (f-delete inpath t))))

(defun org-sql-create-all-erds ()
  (-each '((mysql) (pgsql) (sqlite) (sqlserver)) #'org-sql-write-erd))

;;; table descriptions

(defun org-sql-get-package-version ()
  "Get version of om package."
  (with-current-buffer (find-file-noselect "org-sql.el")
    (->> (package-buffer-info)
         (package-desc-version)
         (-map 'number-to-string)
         (s-join version-separator))))

(defun org-sql-doc-format-quotes (s)
  (s-replace-regexp "`\\([^[:space:]]+\\)'" "`\\1`" s))

(defun org-sql-doc-format-table-row (members)
  (format "| %s |" (s-join " | " members)))

(defun org-sql-doc-format-allowed-values (members)
  (when members
    (-let* ((members* (--map (format "`%s`" it) members))
            ((rest last) (-split-at (1- (length members*)) members*))
            (rest* (s-join ", " rest)))
      (format "%s, or %s" rest* (car last)))))

;; TODO this is a good idea but could be clearer, perhaps a separate column?
;; (defun org-sql-doc-format-org-element-props (desc props)
;;   (cl-flet
;;       ((join-with-and
;;         (xs)
;;         (format "%s and %s" (s-join ", " (-drop-last 1 xs)) (-last-item xs))))
;;     (cond
;;      ((= 0 (length props))
;;       desc)
;;      ((= 1 (length props))
;;       (->> (car props)
;;            (format "%s (corresponds to the org-element `%s` property)" desc )))
;;      ((< 1 (length props))
;;       (->> (--map (format "`%s`" it) props)
;;            (join-with-and)
;;            (format "%s (corresponds to the org-element %s properties)" desc))))))

(defun org-sql-doc-format-column (column-meta constraints-meta)
  (-let* (((column-name . (&plist :desc :type :properties :constraints :allowed))
           column-meta)
          ((&alist 'primary) constraints-meta)
          (primary-keys (plist-get primary :keys))
          (column-name* (org-sql--format-column-name column-name))
          (allowed* (org-sql-doc-format-allowed-values allowed))
          (desc* (-> (if (consp desc) (s-join " " desc) desc)))
                     ;; (org-sql-doc-format-org-element-props properties)))
                   ;; (org-sql-doc-format-quotes it)
                   ;; (if property
                   ;;     (format "%s (corresponds to the org-element `%s` property)"
                   ;;             it property)
                   ;;   it)))
          (desc-full (if allowed* (format "%s (%s)" desc* allowed*) desc*)))
    (->> (list column-name* desc-full)
         (org-sql-doc-format-table-row))))

(defun org-sql-doc-format-schema (table-meta)
  (-let* (((table-name . meta) table-meta)
          ((&alist 'desc 'columns 'constraints) meta)
          (header (->> (symbol-name table-name)
                       (format "## %s")))
          (table-headers (list "Column" "Description"))
          (table-line (->> (-repeat (length table-headers) " - ")
                           (org-sql-doc-format-table-row)))
          (table-headers* (org-sql-doc-format-table-row table-headers))
          (table-rows (->> columns
                           (--map (org-sql-doc-format-column it constraints))
                           (s-join "\n"))))
    (->> (list header
               ""
               (s-join " " desc)
               ""
               table-headers*
               table-line
               table-rows)
         (s-join "\n"))))

(defun create-docs-file ()
  (with-temp-file (f-join org-sql-doc-dir "table-descriptions.md")
    (insert "# Table Descriptions\n\n")

    (insert (->> org-sql--table-alist
                 (-map #'org-sql-doc-format-schema)
                 (s-join "\n\n")))

    (insert (org-sql-get-package-version))))

;;; org-sql-doc.el ends here
