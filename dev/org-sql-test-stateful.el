;;; org-sql-test-stateful.el --- IO tests for org-sql -*- lexical-binding: t; -*-

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

;; These specs test IO functions on org-sql, including reading/writing
;; to databases and reading the status of org files on disk. These are at
;; a higher level of complexity than the stateless tests and thus assume
;; that specification is fully met.

;;; Code:

(require 'org-sql)
(require 's)
(require 'f)
(require 'dash)
(require 'buttercup)

(defconst test-dir (f-dirname (f-this-file)))

(defconst test-files (f-join test-dir "files"))

(defun expect-exit-success (res)
  (-let (((rc . out) res))
    (if (= 0 rc) (expect t)
      (error out))))

(defun org-sql--count-rows (config tbl-name)
  ;; hacky AF...
  (let* ((tbl-name* (org-sql--format-mql-table-name config tbl-name))
         (select (format "SELECT Count(*) FROM %s" tbl-name*)))
    (org-sql--send-sql select)))

(defun expect-db-has-tables (config &rest table-specs)
  (cl-flet
      ((check-table-length
        (tbl-name)
        (let ((out (->> (org-sql--count-rows config tbl-name)
                        (cdr)
                        (s-trim))))
          ;; (print out)
          (if (equal out "") 0 (string-to-number out)))))
    (--each table-specs (expect (check-table-length (car it)) :to-be (cdr it)))))

(defun org-sql--dump-table (config tbl-name)
  ;; TODO this assumes the table in question has no text with newlines
  (-let* (((mode . keyvals) org-sql-db-config)
          ;; TODO make an mql builder function for this
          (select (org-sql--format-mql-select config nil `(,tbl-name))))
    (org-sql--send-sql select)))

(defun expect-db-has-table-contents (config tbl-name &rest rows)
  (cl-flet
      ((test-row-match
        (row-plist row-out)
        (let* ((required-columns (-slice row-plist 0 nil 2))
               (columns (->> (alist-get tbl-name org-sql--mql-tables)
                             (alist-get 'columns)
                             (-map #'car)))
               (row-out-plist
                (->> (org-sql--parse-output-to-plist config columns row-out)
                     (car)
                     (-partition 2)
                     (--filter (memq (car it) required-columns))
                     (-flatten-n 1))))
          (expect row-out-plist :to-equal row-plist))))
    (-let* ((out (->> (org-sql--dump-table config tbl-name)
                      (cdr)
                      (s-trim)
                      (s-lines)
                      (-remove-item ""))))
      (expect (length out) :to-be (length rows))
      (--each (-zip-pair rows out) (test-row-match (car it) (cdr it))))))

(defmacro describe-sql-io-spec (title config)
  (declare (indent 1))
  `(describe ,title
     (before-all
       (setq org-sql-db-config ,config))

     (before-each
       (org-sql--delete-db))

     (after-each
       (org-sql--delete-db))
     
     (it "database exists"
       (expect-exit-success (org-sql--db-create))
       (expect (org-sql--db-exists)))

     (it "database delete"
       (expect-exit-success (org-sql--db-create))
       (org-sql--delete-db)
       (expect (not (org-sql--db-exists))))

     (it "tables exist"
       (expect-exit-success (org-sql-init-db))
       (expect (org-sql--db-has-valid-schema)))

     (it "database update (single file)"
       (let ((org-sql-files (list (f-join test-files "foo1.org"))))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-tables ,config
                               '(file_hashes . 1)
                               '(file_metadata . 1)
                               '(headlines . 1)
                               '(headline_closures . 1))))

     (it "database update (fancy file)"
       (let ((org-sql-files (list (f-join test-files "fancy.org")))
             (org-log-into-drawer "LOGBOOK"))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-tables ,config
                               '(file_hashes . 1)
                               '(file_metadata . 1)
                               '(file_tags . 3)
                               '(file_properties . 1)
                               '(headlines . 5)
                               '(headline_closures . 7)
                               '(planning_entries . 3)
                               '(timestamps . 8)
                               '(links . 1)
                               '(headline_tags . 1)
                               '(headline_properties . 1)
                               '(properties . 2)
                               '(logbook_entries . 5)
                               '(state_changes . 1)
                               '(planning_changes . 4)
                               '(clocks . 1))))

     (it "database update (renamed file)"
       (let* ((path (f-join test-files "foo1.org"))
              (org-sql-files (list path)))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-table-contents ,config 'file_metadata `(:file_path ,path)))
       ;; (print (org-sql--send-sql "SHOW CREATE TABLE headline_closures;"))
       (let* ((path (f-join test-files "foo2.org"))
              (org-sql-files (list path)))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-table-contents ,config 'file_metadata `(:file_path ,path))))
         ;; (expect-db-has-table-contents ,config 'headlines `(:file_path ,path))))

     (it "database update (deleted file)"
       (let ((org-sql-files (list (f-join test-files "foo1.org"))))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db)))
       (let* ((org-sql-files nil))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-tables ,config '(file_metadata . 0))))

     (it "database update (altered file)"
       (let* ((contents1 "* foo1")
              (contents2 "* foo2")
              (path "/tmp/org-sql-test-file.org")
              (org-sql-files (list path)))
         ;; write file and update db
         (f-write-text contents1 'utf-8 path)
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-table-contents ,config 'file_hashes '(:file_hash "ece424e0090cff9b6f1ac50722c336c0"))
         ;; close buffer, alter the file, and update again
         (kill-buffer (find-file-noselect path t))
         (f-write-text contents2 'utf-8 path)
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-table-contents ,config 'file_hashes '(:file_hash "399bc042f23ea976a04b9102c18e9cb5"))
         ;; clean up (yes killing the buffer is necessary)
         (kill-buffer (find-file-noselect path t))
         (f-delete path t)))

     (it "database clear"
       (let ((org-sql-files (list (f-join test-files "foo1.org"))))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-exit-success (org-sql-clear-db))
         (expect-db-has-tables ,config '(file_hashes . 0))
         (expect-db-has-tables ,config '(file_metadata . 0))
         (expect-db-has-tables ,config '(headlines . 0))))

     (it "database reset"
       (expect-exit-success (org-sql--db-create))
       (expect (not (org-sql--db-has-valid-schema)))
       (expect-exit-success (org-sql-reset-db))
       (expect (org-sql--db-has-valid-schema)))

     ))

(describe-sql-io-spec "SQL IO spec (SQLite)"
  '(sqlite :path "/tmp/org-sql-test.db"))

;; (describe-sql-io-spec "SQL IO spec (Postgres)"
;;   '(postgres :database "org_sql"
;;              :port "60001"
;;              :hostname "localhost"
;;              :username "org_sql"
;;              :password "org_sql"))

;; ;; (describe-sql-io-spec "SQL IO spec (Postgres - alt-schema)"
;; ;;   '(postgres :database "org_sql"
;; ;;              :port "60001"
;; ;;              :schema "nonpublic"
;; ;;              :hostname "localhost"
;; ;;              :username "org_sql"
;; ;;              :password "org_sql"))

;; ;; (describe-sql-io-spec "SQL IO spec (MariaDB)"
;; ;;   '(mysql :database "org_sql"
;; ;;           :port "60002"
;; ;;           :hostname "localhost"
;; ;;           :username "org_sql"
;; ;;           :password "org_sql"))

;; (describe-sql-io-spec "SQL IO spec (SQL Server)"
;;   '(sqlserver :database "org_sql"
;;               :port "60003"
;;               :schema "notdbo"
;;               :hostname "localhost"
;;               :username "org_sql"
;;               :password "org_sql333###"))

;;; org-sql-test-stateful ends here
