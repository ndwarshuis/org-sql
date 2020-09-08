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
(require 'buttercup)

(defconst test-dir (f-dirname (f-this-file)))

(defconst test-files (f-join test-dir "files"))

(describe "SQLite formatting spec"
  (it "string conversion (symbol)"
    (expect (org-sql--to-string 'foo) :to-equal "'foo'"))

  (it "string conversion (string)"
    (expect (org-sql--to-string "foo") :to-equal "'foo'"))

  (it "string conversion (number)"
    (expect (org-sql--to-string 1) :to-equal "1"))

  (it "string conversion (keyword)"
    (expect (org-sql--to-string :foo) :to-equal "foo"))

  (it "string conversion (string w/ quotes)"
    (expect (org-sql--to-string "'foo'") :to-equal "'''foo'''"))

  (it "string conversion (string w/ newlines)"
    (expect (org-sql--to-string "foo\nbar") :to-equal "'foo'||char(10)||'bar'"))

  (it "format insert"
    (expect (org-sql--fmt-insert 'foo '(:one 1 :two "2"))
            :to-equal "insert into foo (one,two) values (1,'2');"))

  (it "format update"
    (expect (org-sql--fmt-update 'foo '(:one 1 :two "2") '(:three three))
            :to-equal "update foo set one=1,two='2' where three='three';"))

  (it "format delete"
    (expect (org-sql--fmt-delete 'foo '(:three three))
            :to-equal "delete from foo where three='three';"))

  (it "format delete all"
    (expect (org-sql--fmt-delete-all 'foo)
            :to-equal "delete from foo;"))

  (it "format select"
    (expect (org-sql--fmt-select 'foo '(:a :b))
            :to-equal "select a,b from foo;"))

  (it "format create table (columns only)"
    (let* ((create "CREATE TABLE foo")
           (columns "bar TEXT,bam INTEGER NOT NULL")
           (test-res (format "%s (%s);" create columns))
           (meta '(foo
                   (columns
                    (:bar :type text)
                    (:bam :type integer :constraints (notnull))))))
      (expect (org-sql--meta-create-table meta) :to-equal test-res)))

  (it "format create table (primary)"
    (let* ((create "CREATE TABLE foo")
           (columns "bar TEXT,bam INTEGER NOT NULL")
           (primary "PRIMARY KEY (bar ASC)")
           (test-res (format "%s (%s,%s);" create columns primary))
           (meta '(foo
                   (columns
                    (:bar :type text)
                    (:bam :type integer :constraints (notnull)))
                   (constraints
                    (primary :keys (:bar ASC))))))
      (expect (org-sql--meta-create-table meta) :to-equal test-res)))

  (it "format create table (all)"
    (let* ((create "CREATE TABLE foo")
           (columns "bar TEXT,bam INTEGER NOT NULL")
           (primary "PRIMARY KEY (bar ASC)")
           (foreign "FOREIGN KEY (bam) REFERENCES fubar (BAM)")
           (cascade "ON DELETE CASCADE ON UPDATE CASCADE")
           (test-res (format "%s (%s,%s,%s %s);" create columns primary foreign cascade))
           (meta '(foo
                   (columns
                    (:bar :type text)
                    (:bam :type integer :constraints (notnull)))
                   (constraints
                    (primary :keys (:bar ASC))
                    (foreign :ref fubar
                             :keys (:bam)
                             :parent-keys (:BAM)
                             :on_delete cascade
                             :on_update cascade)))))
      (expect (org-sql--meta-create-table meta) :to-equal test-res))))

(describe "Transaction spec"
  (before-each
    (setq org-sql-sqlite-path "/tmp/org-sql-test.db")
    (f-delete org-sql-sqlite-path t))

  (after-each
    (f-delete org-sql-sqlite-path t))

  (it "classify transactions"
    (let ((on-disk '(("123" . "/bar.org")
                     ("654" . "/bam.org")
                     ("456" . "/foo.org")))
          (in-db '(("123" . "/bar.org")
                   ("654" . "/bam0.org")
                   ("789" . "/foo0.org"))))
      (expect (org-sql--classify-transactions on-disk in-db)
              :to-equal
              '((noops
                 (:hash "123" :disk-path "/bar.org" :db-path "/bar.org"))
                (updates
                 (:hash "654" :disk-path "/bam.org" :db-path "/bam0.org"))
                (inserts
                 (:hash "456" :disk-path "/foo.org" :db-path nil))
                (deletes
                 (:hash "789" :disk-path nil :db-path "/foo0.org"))))))

  (let ((pragma "PRAGMA defer_foreign_keys=on;PRAGMA foreign_keys=on;begin transaction;")
        (commit "commit;"))
    (it "transaction (insert new file)"
      (let* ((org-sql-files (list test-files))
             (test-file (f-join test-files "test.org"))
             (test-md5 "106e9f12c9e4ff3333425115d148fbd4")
             (schema-cmd "CREATE TABLE files (file_path TEXT,md5 TEXT);")
             (files (format "insert into files (file_path,md5,size) values ('%s','%s',6);"
                            test-file test-md5))
             (headlines (format "insert into headlines (file_path,headline_offset,headline_text,keyword,effort,priority,is_archived,is_commented,content) values ('%s',1,'foo',NULL,NULL,NULL,0,0,NULL);insert into headline_closures (file_path,headline_offset,parent_offset,depth) values ('%s',1,1,0);"
                                test-file test-file)))
        (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
                :to-equal "")
        (expect (org-sql--get-transactions)
                :to-equal (concat pragma files headlines commit))))

    (it "transaction (delete file)"
      (let* ((test-file (f-join test-files "nil.org"))
             (schema-cmd "CREATE TABLE files (file_path TEXT,md5 TEXT);")
             (insert-cmd "insert into files (file_path,md5) values ('foo.org','123');")
             (delete "delete from files where file_path='foo.org';"))
        (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
                :to-equal "")
        (expect (org-sql--cmd org-sql-sqlite-path insert-cmd)
                :to-equal "")
        (expect (org-sql--get-transactions)
                :to-equal (concat pragma delete commit))))

    (it "transaction (update file)"
      (let* ((org-sql-files (list test-files))
             (test-file (f-join test-files "test.org"))
             (test-md5 "106e9f12c9e4ff3333425115d148fbd4")
             (schema-cmd "CREATE TABLE files (file_path TEXT,md5 TEXT);")
             (insert-cmd (format "insert into files (file_path,md5) values ('foo','%s');"
                                 test-md5))
             (files (format "update files set file_path='%s' where md5='%s';"
                            test-file test-md5)))
        (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
                :to-equal "")
        (expect (org-sql--cmd org-sql-sqlite-path insert-cmd)
                :to-equal "")
        (expect (org-sql--get-transactions)
                :to-equal (concat pragma files commit))))))

(describe "SQLite command spec"
  (before-each
    (setq org-sql-sqlite-path "/tmp/org-sql-test.db")
    (f-delete org-sql-sqlite-path t))

  (after-each
    (f-delete org-sql-sqlite-path t))

  (let ((schema-cmd "CREATE TABLE FOO (foo TEXT, bar INTEGER);"))
    (it "create SQLite db with schema"
      (org-sql--cmd org-sql-sqlite-path schema-cmd)
      (expect (s-trim (org-sql--cmd org-sql-sqlite-path ".schema"))
              :to-equal schema-cmd))

    (it "add entries to SQLite db and run simple query"
      (expect (org-sql--cmd org-sql-sqlite-path schema-cmd)
              :to-equal "")
      (expect (org-sql--cmd* org-sql-sqlite-path "INSERT INTO FOO (foo,bar) values ('1',2);")
              :to-equal "")
      (expect (s-trim (org-sql--cmd org-sql-sqlite-path "SELECT * FROM FOO;"))
              :to-equal "1|2"))))
