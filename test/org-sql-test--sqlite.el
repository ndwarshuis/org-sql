;;; org-sql-test--sqlite.el --- IO SQLite tests for org-sql -*- lexical-binding: t; -*-

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

(let* ((sqlite (list "SQLite"
                     `(sqlite :path ,(f-join (temporary-file-directory)
                                             "org-sql-test.db")))))
  (eval
   `(describe-io-specs
     ,@sqlite)
   t))

;;; org-sql-test--sqlite ends here
