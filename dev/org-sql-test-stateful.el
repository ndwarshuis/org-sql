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

(defmacro expect-db-has-tables (config header &rest table-specs)
  (declare (indent 1))
  (let ((it-forms
         (--map `(it ,(format "table %s has %s row(s)" (car it) (cdr it))
                   (let ((n (--> (org-sql--count-rows ',config ',(car it))
                              (cdr it)
                              (s-trim it)
                              (if (equal it "") 0 (string-to-number it)))))
                     (expect n :to-be ,(cdr it))))
                table-specs)))
    `(describe "test that correct tables are populated" ,@it-forms)))

(defun org-sql--dump-table (config tbl-name)
  ;; TODO this assumes the table in question has no text with newlines
  (-let* (((mode . keyvals) org-sql-db-config)
          ;; TODO make an mql builder function for this
          (select (org-sql--format-mql-select config nil `(,tbl-name))))
    (org-sql--send-sql select)))

(defun expect-db-has-table-contents (config tbl-name &rest rows)
  (declare (indent 2))
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

(defmacro describe-reset-db (header &rest body)
  (declare (indent 1))
  `(describe ,header
     (after-all
       (org-sql-reset-db))
     ,@body))

(defmacro describe-sql-database-spec (config)
  (let ((it-forms
         (org-sql--case-mode config
           ((mysql postgres sqlserver)
            '((it "create database should error"
                (should-error (org-sql-create-db)))
              (it "drop database should error"
                (should-error (org-sql-drop-db)))
              ;; TODO add a condition where this will return nil?
              (it "database should exist regardless"
                (expect (org-sql-db-exists)))))
           (sqlite
            '((it "create database"
                (expect-exit-success (org-sql-create-db)))
              (it "database should exist"
                (expect (org-sql-db-exists)))
              (it "drop database"
                (expect-exit-success (org-sql-drop-db)))
              (it "database should not exist"
                (expect (not (org-sql-db-exists)))))))))
    `(describe "Database Admin Spec"
       (before-all
         (setq org-sql-db-config ',config))
       (after-all
         (ignore-errors
           (org-sql-drop-db)))
       ,@it-forms)))

(defmacro describe-sql-namespace-spec (config)
  (let ((it-forms
         (org-sql--case-mode config
           ((mysql sqlite)
            '((it "create namespace should error"
                (should-error (org-sql-create-namespace)))
              (it "drop namespace should error"
                (should-error (org-sql-drop-namespace)))
              (it "testing for existence should error"
                (should-error (org-sql-namespace-exists)))))
           ((postgres sqlserver)
            '((it "create namespace"
                (expect-exit-success (org-sql-create-namespace)))
              (it "namespace should exist"
                (expect (org-sql-namespace-exists)))
              (it "drop namespace"
                (expect-exit-success (org-sql-drop-namespace)))
              (it "namespace should not exist"
                (expect (not (org-sql-namespace-exists)))))))))
    `(describe "Namespace Admin Spec"
       (before-all
         (setq org-sql-db-config ',config)
         (ignore-errors
           (org-sql-create-db)))
       (after-all
         (ignore-errors
           (org-sql-drop-namespace))
         (ignore-errors
           (org-sql-drop-db)))
       ,@it-forms)))

(defmacro describe-sql-table-spec (config)
  `(describe "Table Admin Spec"
     (before-all
       (setq org-sql-db-config ',config)
       (ignore-errors
         (org-sql-create-db))
       (ignore-errors
         (org-sql-create-namespace)))
     (after-all
       (ignore-errors
         (org-sql-drop-tables))
       (ignore-errors
         (org-sql-drop-namespace))
       (ignore-errors
         (org-sql-drop-db)))
     (it "create tables"
       (expect-exit-success (org-sql-create-tables)))
     (it "tables should exist"
       (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                    :test #'equal)))
     (it "drop namespace"
       (expect-exit-success (org-sql-drop-tables)))
     (it "tables should not exist"
       (expect (length (org-sql-list-tables)) :to-be 0))))

(defmacro describe-sql-init-spec (config)
  (-let (((it-namespace1 it-namespace2)
          (org-sql--case-mode config
            ((mysql sqlite)
             nil)
            ((postgres sqlserver)
             (let ((x '(expect (org-sql-namespace-exists))))
               (list `((it "namespace exists" ,x))
                     `((it "namespace still exists" ,x))))))))
  `(describe "Initialization/Reset Spec"
     (before-all
       (setq org-sql-db-config ',config))
     (after-all
       (ignore-errors
         (org-sql-drop-tables))
       (ignore-errors
         (org-sql-drop-namespace))
       (ignore-errors
         (org-sql-drop-db)))
     (it "initialize database"
       (expect-exit-success (org-sql-init-db)))
     (it "database should exist"
       (expect (org-sql-db-exists)))
     ,@it-namespace1
     (it "tables should exist"
       (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                    :test #'equal)))
     (it "reset database"
       (expect-exit-success (org-sql-reset-db)))
     (it "database should still exist"
       (expect (org-sql-db-exists)))
     ,@it-namespace2
     (it "tables should still exist"
       (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                    :test #'equal))))))

(defmacro describe-sql-update-spec (config)
  ;; ASSUME init/reset work
  `(describe "Update DB Spec"
     (before-all
       (setq org-sql-db-config ',config)
       (org-sql-init-db))

     (describe-reset-db "single file"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org"))))
           (expect-exit-success (org-sql-update-db))))
       (expect-db-has-tables ,config
         (file_hashes . 1)
         (file_metadata . 1)
         (headlines . 1)
         (headline_closures . 1)))

     (describe-reset-db "two different files"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org")
                                    (f-join test-files "foo3.org"))))
           (expect-exit-success (org-sql-update-db))))
       (expect-db-has-tables ,config
         (file_hashes . 2)
         (file_metadata . 2)
         (headlines . 2)
         (headline_closures . 2)))

     (describe-reset-db "two identical files"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org")
                                    (f-join test-files "foo2.org"))))
           (expect-exit-success (org-sql-update-db))))
       (expect-db-has-tables ,config
         (file_hashes . 1)
         (file_metadata . 2)
         (headlines . 1)
         (headline_closures . 1)))

     (describe-reset-db "fancy file"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "fancy.org")))
               (org-log-into-drawer "LOGBOOK"))
           (expect-exit-success (org-sql-update-db))))
       (expect-db-has-tables ,config
         (file_hashes . 1)
         (file_metadata . 1)
         (file_tags . 3)
         (file_properties . 1)
         (headlines . 5)
         (headline_closures . 7)
         (planning_entries . 3)
         (timestamps . 8)
         (links . 1)
         (headline_tags . 1)
         (headline_properties . 1)
         (properties . 2)
         (logbook_entries . 5)
         (state_changes . 1)
         (planning_changes . 4)
         (clocks . 1)))

     (describe-reset-db "renamed file"
       (describe "insert file"
         (before-all
           (setq test-path (f-join test-files "foo1.org")))
         (it "update database"
           (let ((org-sql-files (list test-path)))
             (expect-exit-success (org-sql-update-db))))
         (it "test for file in tables"
           (expect-db-has-table-contents ',config 'file_metadata
                                         `(:file_path ,test-path))))
       (describe "rename inserted file"
         ;; "rename" here means to point `org-sql-files' to an identical file
         ;; with a different name
         (before-all
           (setq test-path (f-join test-files "foo2.org")))
         (it "update database"
           (let ((org-sql-files (list test-path)))
             (expect-exit-success (org-sql-update-db))))
         (it "test for file in tables"
           (expect-db-has-table-contents ',config 'file_metadata
                                         `(:file_path ,test-path)))))

     (describe-reset-db "deleted file"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org"))))
           (expect-exit-success (org-sql-update-db))))
       (it "update database (untrack the original file)"
         (let ((org-sql-files nil))
           (expect-exit-success (org-sql-update-db))))
       (expect-db-has-tables ,config
         (file_metadata . 0)
         (file_hashes . 0)))

     (describe-reset-db "altered file"
       ;; in order to make this test work, make a file in /tmp and alter
       ;; its contents
       ;; TODO what about windows users? (or people without a /tmp folder?)
       (describe "insert file"
         (before-all
           (setq test-path "/tmp/org-sql-test-file.org"))
         (it "update database"
           (let ((contents1 "* foo1")
                 (org-sql-files (list test-path)))
             ;; write file and update db
             (f-write-text contents1 'utf-8 test-path)
             (expect-exit-success (org-sql-update-db))))
         (it "test file hash"
           (expect-db-has-table-contents ',config 'file_hashes
             '(:file_hash "ece424e0090cff9b6f1ac50722c336c0"))))
       (describe "alter the file"
         (before-all
           (setq test-path "/tmp/org-sql-test-file.org"))
         (it "update with new contents"
           (let ((contents2 "* foo2")
                 (org-sql-files (list test-path)))
             ;; close buffer, alter the file, and update again
             (kill-buffer (find-file-noselect test-path t))
             (f-write-text contents2 'utf-8 test-path)
             (expect-exit-success (org-sql-update-db))))
         (it "test for new file hash"
           (expect-db-has-table-contents ',config 'file_hashes
             '(:file_hash "399bc042f23ea976a04b9102c18e9cb5")))
         (it "clean up"
           ;; yes killing the buffer is necessary
           (kill-buffer (find-file-noselect test-path t))
           (f-delete test-path t))))))

(defmacro describe-sql-clear-spec (config)
  ;; ASSUME init/reset work
  `(describe "Clear DB Spec"
     (before-all
       (setq org-sql-db-config ',config)
       (org-sql-init-db))

     (describe-reset-db "clearing an empty db"
       (it "clear database"
         (expect-exit-success (org-sql-clear-db)))
       (it "tables should still exist"
         (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                      :test #'equal))))

     (describe-reset-db "loading a file and clearing"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org"))))
           (expect-exit-success (org-sql-update-db))))
       (it "clear database"
         (expect-exit-success (org-sql-clear-db)))
       (it "tables should still exist"
         (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                      :test #'equal)))
       (expect-db-has-tables ,config
         (file_hashes . 0)
         (file_metadata . 0)
         (headlines . 0)
         (timestamps . 0)
         (properties . 0)
         (file_properties . 0)
         (headline_properties . 0)
         (file_tags . 0)
         (headline_tags . 0)
         (logbook_entries . 0)
         (planning_changes . 0)
         (state_changes . 0)
         (planning_entries . 0)
         (clocks . 0)))))

(defmacro describe-io-spec (unique-name config)
  (declare (indent 1))
  (let ((title (format "SQL IO Spec (%s)" unique-name)))
    `(describe ,title
       (describe-sql-database-spec ,config)
       (describe-sql-namespace-spec ,config)
       (describe-sql-table-spec ,config)
       (describe-sql-init-spec ,config)
       (describe-sql-update-spec ,config)
       (describe-sql-clear-spec ,config))))

(describe-io-spec "SQLite"
  (sqlite :path "/tmp/org-sql-test.db"))

(describe-io-spec "Postgres"
  (postgres :database "org_sql"
            :port "60001"
            :hostname "localhost"
            :username "org_sql"
            :password "org_sql"))

(describe-io-spec "Postgres: non-default schema"
  (postgres :database "org_sql"
            :port "60001"
            :schema "nonpublic"
            :hostname "localhost"
            :username "org_sql"
            :password "org_sql"))

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
