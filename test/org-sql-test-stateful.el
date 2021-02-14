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

(defconst test-scripts (f-join test-dir "scripts"))

(defun expect-exit-success (res)
  (if org-sql-async
      (let ((out (or (while (accept-process-output res)) ""))
            (status (process-status res)))
        (if (and (eq 'exit status) (equal out "")) (expect t)
          (error out)))
    (-let (((rc . out) res))
      (if (and (= 0 rc) (equal out "")) (expect t)
        (error out)))))

(defun org-sql--count-rows (config tbl-name)
  ;; hacky AF...
  (let* ((tbl-name* (org-sql--format-table-name config tbl-name))
         (select (format "SELECT Count(*) FROM %s" tbl-name*)))
    (org-sql-send-sql select)))

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

(defun expect-db-has-table-contents (tbl-name &rest rows)
  (declare (indent 1))
  (let ((out (->> (org-sql-dump-table tbl-name))))
    (->> (--zip-with (->> (--zip-with (if (functionp it)
                                          (funcall it other)
                                        (equal it other))
                                      it other)
                          (--all? (eq t it)))
                     rows out)
         (--all? (eq t it))
         (expect))))
    ;; (expect out :to-equal rows)))

(defmacro describe-reset-db (header &rest body)
  (declare (indent 1))
  `(describe ,header
     (after-all
       (org-sql-reset-db)
       (org-sql-init-db))
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

(defmacro describe-sql-table-spec (config)
  `(describe "Table Admin Spec"
     (before-all
       (setq org-sql-db-config ',config)
       (ignore-errors
         (org-sql-create-db)))
     (after-all
       (ignore-errors
         (org-sql-drop-tables))
       (ignore-errors
         (org-sql-drop-db)))
     (it "create tables"
       (expect-exit-success (org-sql-create-tables)))
     (it "tables should exist"
       (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                    :test #'equal)))
     (it "drop tables"
       (expect-exit-success (org-sql-drop-tables)))
     (it "tables should not exist"
       (expect (length (org-sql-list-tables)) :to-be 0))))

(defmacro describe-sql-init-spec (config)
  (let ((should-exist (org-sql--case-mode config
                        (sqlite
                         '(it "database should not exist"
                            (expect (not (org-sql-db-exists)))))
                        ((mysql postgres sqlserver)
                         '(it "database should still exist"
                            (expect (org-sql-db-exists)))))))
    (cl-flet
        ((mk-form
          (async)
          (let ((title (concat "Initialization/Reset Spec"
                               (when async " (async)"))))
            `(describe ,title
               (before-all
                 (setq org-sql-db-config ',config))
               (after-all
                 (ignore-errors
                   (org-sql-drop-tables))
                 (ignore-errors
                   (org-sql-drop-db)))
               (it "initialize database"
                 (let ((org-sql-async ,async))
                   (expect-exit-success (org-sql-init-db))))
               (it "database should exist"
                 (expect (org-sql-db-exists)))
               (it "tables should exist"
                 (expect (org-sql--sets-equal org-sql-table-names
                                              (org-sql-list-tables)
                                              :test #'equal)))
               (it "reset database"
                 (let ((org-sql-async ,async))
                   (expect-exit-success (org-sql-reset-db))))
               ,should-exist
               (it "tables should not exist"
                 (expect (not (org-sql--sets-equal org-sql-table-names
                                                   (org-sql-list-tables)
                                                   :test #'equal))))))))
      `(progn
         ,(mk-form nil)
         ,(mk-form t)))))

(defun org-sql-is-number (x)
  (or (null x) (and (stringp x) (s-matches? "[0-9]+" x))))

(defmacro describe-sql-update-spec (config)
  ;; ASSUME init/reset work
  (cl-flet
      ((mk-single
        (async)
        (let ((title (concat "single file" (when async " (async)"))))
          `(describe-reset-db ,title
             (it "update database"
               (let ((org-sql-files (list (f-join test-files "foo1.org")))
                     (org-sql-async ,async))
                 (expect-exit-success (org-sql-push-to-db))))
             (expect-db-has-tables ,config
               (outlines . 1)
               (file_metadata . 1)
               (headlines . 1)
               (headline_closures . 1)
               (planning_entries . 0)
               (timestamps . 0)
               (timestamp_warnings . 0)
               (timestamp_repeaters . 0)
               (links . 0)
               (headline_tags . 0)
               (headline_properties . 0)
               (properties . 0)
               (logbook_entries . 0)
               (state_changes . 0)
               (planning_changes . 0)
               (clocks . 0))))))
  `(describe "Update DB Spec"
     (before-all
       (setq org-sql-db-config ',config)
       (org-sql-init-db))

     (after-all
       (ignore-errors
         (org-sql-reset-db)))

     ,(mk-single nil)
     ,(mk-single t)

     (describe-reset-db "two different files"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org")
                                    (f-join test-files "foo3.org"))))
           (expect-exit-success (org-sql-push-to-db))))
       (expect-db-has-tables ,config
         (outlines . 2)
         (file_metadata . 2)
         (headlines . 2)
         (headline_closures . 2)
         (planning_entries . 0)
         (timestamps . 0)
         (timestamp_warnings . 0)
         (timestamp_repeaters . 0)
         (links . 0)
         (headline_tags . 0)
         (headline_properties . 0)
         (properties . 0)
         (logbook_entries . 0)
         (state_changes . 0)
         (planning_changes . 0)
         (clocks . 0)))

     (describe-reset-db "two identical files"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org")
                                    (f-join test-files "foo2.org"))))
           (expect-exit-success (org-sql-push-to-db))))
       (expect-db-has-tables ,config
         (outlines . 1)
         (file_metadata . 2)
         (headlines . 1)
         (headline_closures . 1)
         (planning_entries . 0)
         (timestamps . 0)
         (timestamp_warnings . 0)
         (timestamp_repeaters . 0)
         (links . 0)
         (headline_tags . 0)
         (headline_properties . 0)
         (properties . 0)
         (logbook_entries . 0)
         (state_changes . 0)
         (planning_changes . 0)
         (clocks . 0)))

     (describe-reset-db "fancy file"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "fancy.org")))
               (org-log-into-drawer "LOGBOOK"))
           (expect-exit-success (org-sql-push-to-db))))
       (expect-db-has-tables ,config
         (outlines . 1)
         (file_metadata . 1)
         (file_tags . 3)
         (headlines . 5)
         (headline_closures . 7)
         (planning_entries . 3)
         (timestamps . 8)
         (timestamp_warnings . 1)
         (timestamp_repeaters . 1)
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
             (expect-exit-success (org-sql-push-to-db))))
         (it "test for file in tables"
           (expect-db-has-table-contents 'file_metadata
             `(,test-path "106e9f12c9e4ff3333425115d148fbd4" org-sql-is-number
                          org-sql-is-number org-sql-is-number org-sql-is-number
                          "-rw-r--r--"))))
       (describe "rename inserted file"
         ;; "rename" here means to point `org-sql-files' to an identical file
         ;; with a different name
         (before-all
           (setq test-path (f-join test-files "foo2.org")))
         (it "update database"
           (let ((org-sql-files (list test-path)))
             (expect-exit-success (org-sql-push-to-db))))
         (it "test for file in tables"
           (expect-db-has-table-contents 'file_metadata
             `(,test-path "106e9f12c9e4ff3333425115d148fbd4" org-sql-is-number
                          org-sql-is-number org-sql-is-number org-sql-is-number
                          "-rw-r--r--")))))

     (describe-reset-db "deleted file"
       (it "update database"
         (let ((org-sql-files (list (f-join test-files "foo1.org"))))
           (expect-exit-success (org-sql-push-to-db))))
       (it "update database (untrack the original file)"
         (let ((org-sql-files nil))
           (expect-exit-success (org-sql-push-to-db))))
       (expect-db-has-tables ,config
         (file_metadata . 0)
         (outlines . 0)))

     (describe-reset-db "altered file"
       ;; in order to make this test work, make a file in /tmp and alter
       ;; its contents
       (describe "insert file"
         (before-all
           (setq test-path (f-join (temporary-file-directory)
                                   "org-sql-test-file.org")))
         (it "update database"
           (let ((contents1 "* foo1")
                 (org-sql-files (list test-path)))
             ;; write file and update db
             (f-write-text contents1 'utf-8 test-path)
             (expect-exit-success (org-sql-push-to-db))))
         (it "test file hash"
           (expect-db-has-table-contents 'outlines
             `("ece424e0090cff9b6f1ac50722c336c0" "6" "1"))))
       (describe "alter the file"
         (before-all
           (setq test-path (f-join (temporary-file-directory)
                                   "org-sql-test-file.org")))
         (it "update with new contents"
           (let ((contents2 "* foo2")
                 (org-sql-files (list test-path)))
             ;; close buffer, alter the file, and update again
             (kill-buffer (find-file-noselect test-path t))
             (f-write-text contents2 'utf-8 test-path)
             (expect-exit-success (org-sql-push-to-db))))
         (it "test for new file hash"
           (expect-db-has-table-contents 'outlines
             `("399bc042f23ea976a04b9102c18e9cb5" "6" "1")))
         (it "clean up"
           ;; yes killing the buffer is necessary
           (kill-buffer (find-file-noselect test-path t))
           (f-delete test-path t)))))))

(defmacro describe-sql-clear-spec (config)
  ;; ASSUME init/reset work
  (cl-flet
      ((mk-clear
        (async)
        (let ((title (concat "loading a file and clearing" (when async " (async)"))))
          `(describe-reset-db ,title
             (it "update database"
               (let ((org-sql-files (list (f-join test-files "foo1.org")))
                     (org-sql-async ,async))
                 (expect-exit-success (org-sql-push-to-db))))
             (it "clear database"
               (let ((org-sql-async ,async))
                 (expect-exit-success (org-sql-clear-db))))
             (it "tables should still exist"
               (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                            :test #'equal)))
             (expect-db-has-tables ,config
               (outlines . 0)
               (file_metadata . 0)
               (headlines . 0)
               (timestamps . 0)
               (timestamp_warnings . 0)
               (timestamp_repeaters . 0)
               (properties . 0)
               (headline_properties . 0)
               (file_tags . 0)
               (headline_tags . 0)
               (logbook_entries . 0)
               (planning_changes . 0)
               (state_changes . 0)
               (planning_entries . 0)
               (clocks . 0))))))

  `(describe "Clear DB Spec"
     (before-all
       (setq org-sql-db-config ',config)
       (org-sql-init-db))

     (after-all
       (ignore-errors
         (org-sql-reset-db)))

     (describe-reset-db "clearing an empty db"
       (it "clear database"
         (expect-exit-success (org-sql-clear-db)))
       (it "tables should still exist"
         (expect (org-sql--sets-equal org-sql-table-names (org-sql-list-tables)
                                      :test #'equal))))

     ,(mk-clear nil)
     ,(mk-clear t))))

(defmacro describe-sql-hook-spec (config)
  `(describe "DB Hook Spec"
     (before-all
       (setq org-sql-db-config ',config)
       (ignore-errors
         (org-sql-drop-tables))
       (ignore-errors
         (org-sql-drop-db)))

     (after-all
       (org-sql-send-sql "DROP TABLE IF EXISTS fake_init_table;")
       (org-sql-send-sql "DROP TABLE IF EXISTS fake_update_table;")
       (org-sql-send-sql "DROP TABLE IF EXISTS save_something;")
       (ignore-errors
         (org-sql-reset-db)))

     (it "init database"
       (let ((org-sql-post-init-hooks
              `((file ,(f-join test-scripts "init_hook.sql"))
                (sql "INSERT INTO fake_init_table VALUES (1);"))))
         (expect-exit-success (org-sql-init-db))))
     (it "fake init table should exist"
       (expect-db-has-table-contents 'fake_init_table '("1")))
     (it "update database"
       (let ((org-sql-post-push-hooks
              `((file+ ,(f-join test-scripts "update_hook.sql"))
                (sql+ "INSERT INTO fake_update_table VALUES (1);")))
             (org-sql-files (list (f-join test-files "foo1.org"))))
         (expect-exit-success (org-sql-push-to-db))))
     (it "fake update table should exist"
       (expect-db-has-table-contents 'fake_update_table '("1")))
     (it "clear database"
       (let ((org-sql-post-clear-hooks
              `((file ,(f-join test-scripts "clear_hook.sql"))
                (sql "DROP TABLE fake_update_table;"))))
         (expect-exit-success (org-sql-clear-db))))
     (it "fake init table should not exist"
       (expect (not (member "fake_init_table" (org-sql-list-tables)))))
     (it "fake update table should not exist"
       (expect (not (member "fake_update_table" (org-sql-list-tables)))))
     (it "reset database"
       (let ((org-sql-pre-reset-hooks
              `((sql "CREATE TABLE save_something (x INTEGER);"))))
         (expect-exit-success (org-sql-reset-db))))
     (it "reset table should exist"
       (expect (member "save_something" (org-sql-list-tables))))))

(defmacro describe-io-spec (unique-name config)
  (declare (indent 1))
  ;; this spec only works with the default schema; I suppose if I was less lazy
  ;; I could make it work for non-default schema's but it's nice when the same
  ;; SQL statements work for all tests and configs ;)
  (let ((hook-spec (org-sql--with-config-keys (:schema) config
                     (unless schema `((describe-sql-hook-spec ,config))))))
                   
  `(describe ,unique-name
     (after-all
       (ignore-errors
         (org-sql-drop-tables))
       (ignore-errors
         (org-sql-drop-db)))
     (describe-sql-database-spec ,config)
     (describe-sql-table-spec ,config)
     (describe-sql-init-spec ,config)
     (describe-sql-update-spec ,config)
     (describe-sql-clear-spec ,config)
     ,@hook-spec)))

(defmacro describe-io-specs (&rest specs)
  (declare (indent 0))
  (let ((forms (->> (-partition 2 specs)
                    (--map `(describe-io-spec ,(car it) ,(cadr it))))))
    `(describe "SQL IO Spec"
       ,@forms)))

(cl-flet*
    ((mk-io-spec
      (db-name db-sym version alt-title key-vals)
      `(,(if alt-title (format "%s (v%s - %s)" db-name version alt-title)
           (format "%s (v%s)" db-name version))
        (,db-sym ,@key-vals)))
     (mk-postgres
      (version port &optional alt-title key-vals)
      (->> (list :database "org_sql"
                 :port port
                 :hostname "localhost"
                 :username "org_sql"
                 :password "org_sql")
           (append key-vals)
           (mk-io-spec "Postgres" 'postgres version alt-title)))
     (mk-mysql
      (title version port &optional alt-title key-vals)
      (->> (list :database "org_sql"
                 :port port
                 :hostname "127.0.0.1"
                 :username "org_sql"
                 :password "org_sql")
           (append key-vals)
           (mk-io-spec title 'mysql version alt-title)))
     (mk-sqlserver
      (version port &optional alt-title key-vals)
      (->> (list :database "org_sql"
                 :server (format "tcp:localhost,%s" port)
                 :username "org_sql"
                 :password "org_sql333###")
           (append key-vals)
           (mk-io-spec "SQL-Server" 'sqlserver version alt-title))))
  (let* ((sqlite (list "SQLite"
                       `(sqlite :path ,(f-join (temporary-file-directory)
                                               "org-sql-test.db"))))
         (postgres
          (append
           (mk-postgres 13 60013)
           (mk-postgres 13 60013 "Non-Default Schema" '(:schema "nonpublic"))
           (mk-postgres 13 60013 "Unlogged tables" '(:unlogged t))
           (mk-postgres 12 60012)
           (mk-postgres 11 60011)
           (mk-postgres 10 60010)
           (mk-postgres 9 60009)))
         (mariadb
          (append
           (mk-mysql "MariaDB" 10.5 60105)
           (mk-mysql "MariaDB" 10.4 60104)
           (mk-mysql "MariaDB" 10.3 60103)
           (mk-mysql "MariaDB" 10.2 60102)))
         (mysql
          (append
           (mk-mysql "MySQL" 8.0 60280)
           (mk-mysql "MySQL" 5.7 60257)))
         (sqlserver
          (append
           (mk-sqlserver 2019 60319 nil '(:schema "nondbo"))
           (mk-sqlserver 2017 60317 nil '(:schema "nondbo")))))
  (eval
   `(describe-io-specs
      ,@sqlite
      ,@postgres
      ,@mariadb
      ,@mysql
      ,@sqlserver))))

;;; org-sql-test-stateful ends here
