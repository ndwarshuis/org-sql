;;; org-sql-doc.el --- SQLite tests for org-sql -*- lexical-binding: t; -*-

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

;; These specs test SQLite-specific code in org-sql

;;; Code:

(require 'org-sql)
(require 's)
(require 'f)
(require 'dash)
(require 'buttercup)

(defconst test-dir (f-dirname (f-this-file)))

(defconst test-files (f-join test-dir "files"))

(describe "Transaction spec"
  (it "classify transactions"
    (let ((on-disk (list (org-sql--to-fmeta "/bar.org" nil "123")
                         (org-sql--to-fmeta "/bam.org" nil "654")
                         (org-sql--to-fmeta "/foo.org" nil "456")))
          (in-db (list (org-sql--to-fmeta nil "/bar.org" "123")
                       (org-sql--to-fmeta nil "/bam0.org" "654")
                       (org-sql--to-fmeta nil "/foo0.org" "789"))))
      (expect (org-sql--classify-fmeta on-disk in-db)
              :to-equal
              `((deletes
                 ,(org-sql--to-fmeta nil "/foo0.org" "789"))
                (updates
                 ,(org-sql--to-fmeta "/bam.org" "/bam0.org" "654"))
                (inserts
                 ,(org-sql--to-fmeta "/foo.org" nil "456"))
                (noops
                 ,(org-sql--to-fmeta "/bar.org" "/bar.org" "123")))))))

;;   (let ((pragma "PRAGMA defer_foreign_keys=on;PRAGMA foreign_keys=on;begin transaction;")
;;         (commit "commit;"))
;;     (it "transaction (insert new file)"
;;       (let* ((org-sql-files (list test-files))
;;              (test-file (f-join test-files "test.org"))
;;              (test-md5 "106e9f12c9e4ff3333425115d148fbd4")
;;              (schema-cmd "CREATE TABLE files (file_path TEXT,md5 TEXT);")
;;              (files (format "insert into files (file_path,md5,size) values ('%s','%s',6);"
;;                             test-file test-md5))
;;              (headlines (format "insert into headlines (file_path,headline_offset,headline_text,keyword,effort,priority,is_archived,is_commented,content) values ('%s',1,'foo',NULL,NULL,NULL,0,0,NULL);insert into headline_closures (file_path,headline_offset,parent_offset,depth) values ('%s',1,1,0);"
;;                                 test-file test-file)))
;;         (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
;;                 :to-equal "")
;;         (expect (org-sql--get-transactions)
;;                 :to-equal (concat pragma files headlines commit))))

;;     (it "transaction (delete file)"
;;       (let* ((test-file (f-join test-files "nil.org"))
;;              (schema-cmd "CREATE TABLE files (file_path TEXT,md5 TEXT);")
;;              (insert-cmd "insert into files (file_path,md5) values ('foo.org','123');")
;;              (delete "delete from files where file_path='foo.org';"))
;;         (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
;;                 :to-equal "")
;;         (expect (org-sql--cmd org-sql-sqlite-path insert-cmd)
;;                 :to-equal "")
;;         (expect (org-sql--get-transactions)
;;                 :to-equal (concat pragma delete commit))))

;;     (it "transaction (update file)"
;;       (let* ((org-sql-files (list test-files))
;;              (test-file (f-join test-files "test.org"))
;;              (test-md5 "106e9f12c9e4ff3333425115d148fbd4")
;;              (schema-cmd "CREATE TABLE files (file_path TEXT,md5 TEXT);")
;;              (insert-cmd (format "insert into files (file_path,md5) values ('foo','%s');"
;;                                  test-md5))
;;              (files (format "update files set file_path='%s' where md5='%s';"
;;                             test-file test-md5)))
;;         (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
;;                 :to-equal "")
;;         (expect (org-sql--cmd org-sql-sqlite-path insert-cmd)
;;                 :to-equal "")
;;         (expect (org-sql--get-transactions)
;;                 :to-equal (concat pragma files commit))))))

;; (describe "SQLite command spec"
;;   (before-each
;;     (setq org-sql-sqlite-path "/tmp/org-sql-test.db")
;;     (f-delete org-sql-sqlite-path t))

;;   (after-each
;;     (f-delete org-sql-sqlite-path t))

;;   (let ((schema-cmd "CREATE TABLE FOO (foo TEXT, bar INTEGER);"))
;;     (it "create SQLite db with schema"
;;       (org-sql--cmd org-sql-sqlite-path schema-cmd)
;;       (expect (s-trim (org-sql--cmd org-sql-sqlite-path ".schema"))
;;               :to-equal schema-cmd))

;;     (it "add entries to SQLite db and run simple query"
;;       (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
;;               :to-equal "")
;;       (expect (org-sql--cmd* org-sql-sqlite-path "INSERT INTO FOO (foo,bar) values ('1',2);")
;;               :to-equal "")
;;       (expect (s-trim (org-sql--cmd org-sql-sqlite-path "SELECT * FROM FOO;"))
;;               :to-equal "1|2"))))

(defun expect-exit-success (res)
  (-let (((rc . out) res))
    (if (= 0 rc) (expect t)
      (error out))))

(defun org-sql--count-rows (tbl-name)
  ;; hacky AF...
  (let ((select (format "SELECT Count(*) FROM %s" tbl-name)))
    (org-sql--send-sql select)))

(defun expect-db-has-tables (&rest table-specs)
  (cl-flet
      ((check-table-length
        (tbl-name)
        (let ((out (->> (org-sql--count-rows tbl-name)
                        (cdr)
                        (s-trim))))
          ;; (print out)
          (if (equal out "") 0 (string-to-number out)))))
    (--each table-specs (expect (check-table-length (car it)) :to-be (cdr it)))))

(defun org-sql--dump-table (tbl-name)
  ;; TODO this assumes the table in question has no text with newlines
  (-let* (((mode . keyvals) org-sql-db-config)
          ;; TODO make an mql builder function for this
          (select (org-sql--format-mql-select nil `(,tbl-name))))
    (org-sql--send-sql select)))


(defun expect-db-has-table-contents (tbl-name &rest rows)
  (cl-flet
      ((test-row-match
        (row-plist row-out)
        (let* ((required-columns (-slice row-plist 0 nil 2))
               (columns (->> (alist-get tbl-name org-sql--mql-schema)
                             (alist-get 'columns)
                             (-map #'car)))
               (row-out-plist
                (->> (org-sql--parse-output-to-plist columns row-out)
                     (car)
                     (-partition 2)
                     (--filter (memq (car it) required-columns))
                     (-flatten-n 1))))
          (expect row-plist :to-equal row-out-plist))))
    (-let* ((out (->> (org-sql--dump-table tbl-name)
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
         (expect-db-has-tables '(files . 1)
                               '(headlines . 1)
                               '(headline_closures . 1))))

     (it "database update (fancy file)"
       (let ((org-sql-files (list (f-join test-files "fancy.org")))
             (org-log-into-drawer "LOGBOOK"))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-tables '(files . 1)
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
       (let ((org-sql-files (list (f-join test-files "foo1.org"))))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db)))
       (let* ((path (f-join test-files "foo2.org"))
              (org-sql-files (list path)))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-table-contents 'files `(:file_path ,path))))

     (it "database update (deleted file)"
       (let ((org-sql-files (list (f-join test-files "foo1.org"))))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db)))
       (let* ((org-sql-files nil))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-tables '(files . 0))))

     (it "database update (altered file)"
       (let* ((contents1 "* foo1")
              (contents2 "* foo2")
              (path "/tmp/org-sql-test-file.org")
              (org-sql-files (list path)))
         ;; write file and update db
         (f-write-text contents1 'utf-8 path)
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-table-contents 'files '(:md5 "ece424e0090cff9b6f1ac50722c336c0"))
         ;; close buffer, alter the file, and update again
         (kill-buffer (find-file-noselect path t))
         (f-write-text contents2 'utf-8 path)
         (expect-exit-success (org-sql-update-db))
         (expect-db-has-table-contents 'files '(:md5 "399bc042f23ea976a04b9102c18e9cb5"))
         ;; clean up (yes killing the buffer is necessary)
         (kill-buffer (find-file-noselect path t))
         (f-delete path t)))

     (it "database clear"
       (let ((org-sql-files (list (f-join test-files "foo1.org"))))
         (expect-exit-success (org-sql-init-db))
         (expect-exit-success (org-sql-update-db))
         (expect-exit-success (org-sql-clear-db))
         (expect-db-has-tables '(files . 0))))

     (it "database reset"
       (expect-exit-success (org-sql--db-create))
       (expect (not (org-sql--db-has-valid-schema)))
       (expect-exit-success (org-sql-reset-db))
       (expect (org-sql--db-has-valid-schema)))

     ))

(describe-sql-io-spec "SQL IO spec (SQLite)"
  '(sqlite :path "/tmp/org-sql-test.db"))

(describe-sql-io-spec "SQL IO spec (Postgres)"
  '(postgres :database "org_sql_testing"))
