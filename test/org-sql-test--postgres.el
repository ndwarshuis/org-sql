;;; org-sql-test--postgres.el --- IO postgres tests for org-sql -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nathan Dwarshuis

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

(cl-flet*
    ((mk-postgres
      (version port &optional alt-title key-vals)
      (->> (list :database "org_sql"
                 :port port
                 :hostname "localhost"
                 :username "org_sql"
                 :password "org_sql")
           (append key-vals)
           (make-io-spec "Postgres" 'postgres version alt-title))))
  (let* ((postgres
          (append
           (mk-postgres 16 60016)
           (mk-postgres 16 60016 "Non-Default Schema" '(:schema "nonpublic"))
           (mk-postgres 16 60016 "Unlogged tables" '(:unlogged t))
           (mk-postgres 15 60015)
           (mk-postgres 14 60014)
           (mk-postgres 13 60013))))
  (eval
   `(describe-io-specs
      ,@postgres)
   t)))

;;; org-sql-test--postgres ends here
