;;; org-sql.el --- Org-Mode SQL converter -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, data
;; Homepage: https://github.com/ndwarshuis/org-sql
;; Package-Requires: ((emacs "27.1") (s "1.12") (dash "2.15") (org-ml "3.0.0"))
;; Version: 0.0.1

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

;; This library converts org files to tabular data and inserts this
;; into a SQL database. For the moment only SQLite is supported. In
;; addition to the elisp dependencies required here, this library
;; also requires the sqlite3 program to be installed.

;; See README for the structure of the database and the data that is
;; stored in each table.

;; Before data acquisition, each file is checked against the current
;; database using its MD5 checksum to determine if updates are needed.
;; Any required data is obtained by parsing each desired org(archive)
;; file into a tree-structure using `org-element-parse-buffer', and
;; converting this to a series of SQL insert commands to be executed
;; via bulk transactions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 's)
(require 'sql)
(require 'org)
(require 'org-ml)

;;; constants and customizations

(defconst org-sql--ignored-properties-default
  '("ARCHIVE_ITAGS" "Effort")
  "Property keys to be ignored when inserting in properties table.
It is assumed these are used elsewhere and thus it would be redundant
to store them. This is in addition to any properties specifified by
`nd/org-sql-ignored-properties'.")

;; TODO, make a formating function to convert a lisp obj to schema
(defconst org-sql--schemas
  '("CREATE TABLE files (file_path TEXT PRIMARY KEY ASC,md5 TEXT NOT NULL,size INTEGER NOT NULL,time_modified INTEGER,time_created INTEGER,time_accessed INTEGER);"
    "CREATE TABLE headlines (file_path TEXT, headline_offset INTEGER, tree_path TEXT, headline_text TEXT NOT NULL, keyword TEXT, effort INTEGER, priority CHAR, archived BOOLEAN, commented BOOLEAN, content TEXT, PRIMARY KEY (file_path ASC, headline_offset ASC), FOREIGN KEY (file_path) REFERENCES files (file_path) ON UPDATE CASCADE ON DELETE CASCADE);"
    "CREATE TABLE tags (file_path TEXT,headline_offset INTEGER,tag TEXT,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path, headline_offset, tag, inherited));"
    "CREATE TABLE properties (file_path TEXT,headline_offset INTEGER,property_offset INTEGER,key_text TEXT NOT NULL,val_text TEXT NOT NULL,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, property_offset ASC));"
    "CREATE TABLE clocking (file_path TEXT,headline_offset INTEGER,clock_offset INTEGER,time_start INTEGER,time_end INTEGER,clock_note TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset)ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, clock_offset ASC));"
    "CREATE TABLE logbook (file_path TEXT,headline_offset INTEGER,entry_offset INTEGER,entry_type TEXT,time_logged INTEGER,header TEXT,note TEXT,FOREIGN KEY (file_path, headline_offset)REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
    "CREATE TABLE state_changes (file_path TEXT,entry_offset INTEGER,state_old TEXT NOT NULL,state_new TEXT NOT NULL,FOREIGN KEY (file_path, entry_offset) REFERENCES logbook (file_path, entry_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
    "CREATE TABLE planning_changes (file_path TEXT, entry_offset INTEGER, timestamp_offset INTEGER NOT NULL, FOREIGN KEY (file_path, entry_offset) REFERENCES logbook (file_path, entry_offset) ON DELETE CASCADE ON UPDATE CASCADE, PRIMARY KEY (file_path ASC, entry_offset ASC), FOREIGN KEY (file_path, timestamp_offset) REFERENCES timestamp (file_path, timestamp_offset) ON DELETE CASCADE ON UPDATE CASCADE);"
    "CREATE TABLE links (file_path TEXT,headline_offset INTEGER,link_offset INTEGER,link_path TEXT,link_text TEXT,link_type TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, link_offset ASC));"
    "CREATE TABLE timestamp (file_path TEXT, headline_offset INTEGER, timestamp_offset INTEGER, raw_value TEXT NOT NULL, type TEXT, planning_type TEXT, warning_type TEXT, warning_value INTEGER, warning_unit TEXT, repeat_type TEXT, repeat_value INTEGER, repeat_unit TEXT, time INTEGER NOT NULL, time_end INTEGER, resolution TEXT, resolution_end TEXT, PRIMARY KEY (file_path, timestamp_offset), FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON DELETE CASCADE ON UPDATE CASCADE);")
  "Table schemas for the org database.")

(defconst org-sql--default-pragma
  '(:foreign_keys on)
  "Pragma activated upon opening a new SQLite connection.")

(defgroup org-sql nil
  "Org mode SQL backend options."
  :tag "Org SQL"
  :group 'org)
  
(defcustom org-sql-use-tag-inheritance nil
  "Use tag inheritance when constructing sql databases for org.
Mirrors behavior of `org-use-tag-inheritance'."
  :type 'boolean
  :group 'org-sql)

(defcustom org-sql-sqlite-path (expand-file-name "org.db" org-directory)
  "Path for the sqlite database where org data will be stored."
  :type 'file
  :group 'org-sql)

(defcustom org-sql-files nil
  "A list of org files or directories to put into sql database.
Any directories in this list imply that all files within the
directly are added. Only files ending in .org or .org_archive are
considered. See function `org-sql-files'."
  :type '(repeat :tag "List of files and directories" file)
  :group 'org-sql)
  
(defcustom org-sql-pragma
  '(:synchronous off :journal_mode memory)
  "User-defined pragmas used when opening a new SQLite connection.
These cannot override pragma in `org-sql--default-pragma'."
  :type '(plist :key-type symbol :value-type string)
  :group 'org-sql)

(defcustom org-sql-buffer "*SQL: Org*"
  "Name of the SQLi process buffer connected to the database."
  :type 'string
  :group 'org-sql)

(defcustom org-sql-ignored-properties nil
  "List of properties to ignore when building the properties table.
To ignore all set to 'all' instead of a list of strings."
  :type '(choice
          (const "Ignore All" all)
          (repeat :tag "List of properties to ignore" string))
  :group 'org-sql)

(defcustom org-sql-ignored-tags nil
  "List of tags to ignore when building the tags table.
To ignore all set to 'all' instead of a list of strings."
  :type '(choice
          (const "Ignore All" all)
          (repeat :tag "List of tags to ignore" string))
  :group 'org-sql)

(defcustom org-sql-ignored-link-types nil
  "List of link types to ignore when building the links table.
Each member should be a string and one of `org-link-types' or
\"file\", \"coderef\", \"custom-id\", \"fuzzy\", or \"id\". See org-element
API documentation or`org-element-link-parser' for details.
To ignore all set to 'all' instead of a list of strings."
  :type '(choice
          (set :tag "List of types to ignore"
               (const :tag "File paths" "file")
               (const :tag "Source code references" "coderef")
               (const :tag "Headline custom IDs" "custom-id")
               (const :tag "Fuzzy target in parse trees" "fuzzy")
               (const :tag "Headline IDs" "id")
               (repeat :tag "Other types to ignore" string))
          (const "Ignore all" all))
  :group 'org-sql)

(defcustom org-sql-included-headline-planning-types
  '(:deadline :scheduled :closed)
  "List of headline planning timestamps to include in the database.
Must be symbols that are one of ':deadline', ':scheduled', or
':closed'. To include none set to nil."
  :type '(set :tag "List of types to include"
              (const :tag "Deadline Timestamps" :deadline)
              (const :tag "Scheduled Timestamps" :scheduled)
              (const :tag "Closed Timestamps" :closed))
  :group 'org-sql)

(defcustom org-sql-included-contents-timestamp-types
  '(active active-range inactive inactive-range)
  "List of timestamp types to include from headline content sections.
List members are symbols of any 'active', 'active-range', 'inactive',
or 'inactive-range'. To include none set to nil."
  :type '(set :tag "List of types to include"
              (const :tag "Active Timestamps" active)
              (const :tag "Active Timestamp Ranges" active-range)
              (const :tag "Inactive Timestamps" inactive)
              (const :tag "Inactive Timestamp Ranges" inactive-range))
  :group 'org-sql)

(defcustom org-sql-included-logbook-types
  '(done state note reschedule delschedule redeadline deldeadline
         refile)
  "List of logbook entry types to include in the database.
List members are any of the keys from `org-log-note-headings' with the
exception of 'clock-out' as these are treated as clock-notes (see
`org-sql-store-clock-notes'). To include none set to nil."
  :type '(set :tag "List of types to include"
              (const :tag "Closing notes" done)
              (const :tag "State changes" state)
              (const :tag "Notes taken" note)
              (const :tag "Rescheduled tasks" reschedule)
              (const :tag "Unscheduled tasks" delschedule)
              (const :tag "Redeadlined tasks" redeadline)
              (const :tag "Undeadlined tasks" deldeadline)
              (const :tag "Refiled tasks" refile))
  :group 'org-sql)

(defcustom org-sql-store-clocks t
  "Set to t to store clocks in the database."
  :type 'boolean
  :group 'org-sql)

(defcustom org-sql-store-clock-notes t
  "Set to t to store clock notes in the database.
Setting `org-sql-store-clocks' to nil will cause this variable to be
ignored."
  :type 'boolean
  :group 'org-sql)

(defcustom org-sql-debug nil
  "Set to t to enable high-level debugging of SQL transactions."
  :type 'boolean)

;; TODO add a debug buffer
;; (defconst org-sql-debug-buffer "*SQL: Org-Debug*"
;;   "Name of the SQLi buffer connected to the database.")

;;; helper functions

(defmacro org-sql--with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body)) (indent 1))
  `(progn
     ,@(--map (cons #'advice-add it) adlist)
     (unwind-protect (progn ,@body)
       ,@(--map `(advice-remove ,(car it) ,(nth 2 it)) adlist))))

(defun org-sql--plist-get-keys (plist)
  "Return all the keys in PLIST."
  (-slice plist 0 nil 2))

(defun org-sql--plist-get-vals (plist)
  "Return all the values in PLIST."
  (-slice plist 1 nil 2))

;;; SQL string parsing functions

(defun org-sql--to-plist (out cols)
  "Parse SQL output string OUT to an plist representing the data.
COLS are the column names as symbols used to obtain OUT."
  (unless (equal out "")
    (-->
     out
     (string-trim it)
     (split-string it "\n")
     (mapcar (lambda (s) (split-string s "|")) it)
     (mapcar (lambda (s) (-interleave cols s)) it))))

;;; SQL formatting helper functions

(defun org-sql--escape-text (txt)
  "Escape and quote TXT for insertion into SQL database.
This assumes the insertion command will be run on a shell where the
sql command string is in double quotes."
  (->> txt
       (replace-regexp-in-string "'" "''")
       (replace-regexp-in-string "\n" "'||char(10)||'")
       (format "'%s'")))

(defun org-sql--to-string (entry)
  "Convert ENTRY to a string suitable for insertion into SQLite db.
Converts numbers to strings, flanks strings with '\"', and converts
any other symbols to their symbol name."
  (cond ((null entry) "NULL")
        ((stringp entry) (org-sql--escape-text entry))
        ((numberp entry) (number-to-string entry))
        ((keywordp entry) (org-sql--kw-to-colname entry))
        ((symbolp entry) (-> entry symbol-name org-sql--escape-text))
        (t (error "Cannot convert to string: %s" entry))))

;; TODO this name is too specific
;; TODO this now seems redundant
(defun org-sql--kw-to-colname (kw)
  "Return string representation of KW for column in sql database."
  (if (keywordp kw) (--> kw (symbol-name it) (substring it 1))
    (error "Not a keyword: %s" kw)))

(defun org-sql--plist-concat (plist &optional sep)
  "Concatenate a PLIST to string to be used in a SQL statement.
Returns a string formatted like 'prop1 = value1 SEP prop2 = value2'
from a plist like '(:prop1 value1 :prop2 value2)."
  (let ((sep (or sep ","))
        (keys (->> plist
                   org-sql--plist-get-keys
                   (mapcar #'org-sql--kw-to-colname)))
        (vals (->> plist
                   org-sql--plist-get-vals
                   (mapcar #'org-sql--to-string))))
    (-some-->
     (--zip-with (format "%s=%s" it other) keys vals)
     (string-join it sep))))

;;; SQL command formatting functions

(defun org-sql--fmt-insert (tbl-name tbl-data)
  "Format SQL insert command from TBL-NAME and TBL-DATA."
  (let ((col-names (-->
                    tbl-data
                    (org-sql--plist-get-keys it)
                    (mapcar #'org-sql--kw-to-colname it)
                    (string-join it ",")))
        (col-values (-->
                     tbl-data
                     (org-sql--plist-get-vals it)
                     (mapcar #'org-sql--to-string it)
                     (string-join it ","))))
    (format "insert into %s (%s) values (%s);" (symbol-name tbl-name)
            col-names col-values)))

(defun org-sql--fmt-update (tbl-name update)
  "Format SQL update command from TBL-NAME and UPDATE.
UPDATE is a list of plists where each plist represents one row of data
where the properties are the column names."
  (let ((upd-str (->> update car org-sql--plist-concat))
        (conds-str (--> update (cdr it) (org-sql--plist-concat it " and "))))
    (format "update %s set %s where %s;" (symbol-name tbl-name)
            upd-str conds-str)))

(defun org-sql--fmt-delete (tbl-name conds &optional delete-all)
  "Format SQL update command from TBL-NAME and CONDS.
To delete everything in TBL-NAME, supply nil for CONDS and set
DELETE-ALL to t (the latter is a safety mechanism as this could be
very destructive)."
  (let ((tbl-name-str (symbol-name tbl-name)))
    (cond
     (conds
      (--> conds
           (org-sql--plist-concat it " and ")
           (format "delete from %s where %s;" tbl-name-str it)))
     (delete-all
      (format "delete from %s;" tbl-name-str)))))

(defun org-sql--fmt-trans (sql-str &optional newlines)
  "Format SQL transactions string.
SQL-STR is a list of individual SQL commands to be included in the
transaction. If NEWLINES is non-nil, add newlines between all SQL
commands."
  (let ((sep (and newlines "\n"))
        (fmt (if newlines "begin transaction;\n%s\ncommit;"
               "begin transaction; %s commit;"))
        (pragma (concat "pragma defer_foreign_keys=on;"
                        (when newlines "\n"))))
             
    (-some--> (-flatten sql-str)
              (reverse it)
              (string-join it sep)
              (format fmt it)
              ;; turn on deferred keys for all transactions
              (concat pragma it))))

(defun org-sql--fmt-multi (tbl fun)
  "Format multiple SQL command strings.
TBL is tree structure containing the data to insert and
FUN is a function used to format the data in TBL."
  (--map (funcall fun (car tbl) it) (cdr tbl)))

(defun org-sql--fmt-inserts (tbl)
  "Format data in TBL as SQL insert commands.
Returns a list of formatted strings."
  (org-sql--fmt-multi tbl #'org-sql--fmt-insert))

(defun org-sql--fmt-updates (tbl)
  "Format data in TBL as SQL update commands.
Returns a list of formatted strings."
  (org-sql--fmt-multi tbl #'org-sql--fmt-update))

(defun org-sql--fmt-deletes (tbl)
  "Format data in TBL as SQL delete commands.
Returns a list of formatted strings."
  (org-sql--fmt-multi tbl #'org-sql--fmt-delete))

(defun org-sql--fmt-pragma (plist)
  "Create a SQL statement for setting pragmas in PLIST.
PLIST contains the pragmas as the properties and their intended
values as the property values."
  (let ((pragmas (->> plist
                      org-sql--plist-get-keys
                      (mapcar #'org-sql--kw-to-colname))))
    (->> plist
         org-sql--plist-get-vals
         (--zip-with (format "PRAGMA %s=%s;" it other) pragmas)
         string-join)))
  
;;; SQL command abstractions

(defun org-sql-cmd-open-connection ()
  "Open a new SQL connection to `org-sql-sqlite-path'.
This also sets the pragma according to `org-sql--default-pragma'
and `org-sql-pragma'. The process buffer is named `org-sql-buffer'."
  (unless (get-buffer-process org-sql-buffer)
    (org-sql--with-advice
        ((#'sql-get-login :override #'ignore)
         (#'pop-to-buffer :override #'ignore))
      (let ((sql-database org-sql-sqlite-path))
        (sql-sqlite org-sql-buffer)
        (org-sql--cmd-set-pragma org-sql-pragma)))))

(defun org-sql-cmd-kill-connection ()
  "Close the SQL connections to `org-sql-sqlite-path' if it exists."
  (let ((proc (get-buffer-process org-sql-buffer)))
    (when proc
      (set-process-query-on-exit-flag proc nil)
      (kill-process proc)
      (while (eq 'run (process-status proc))
        (sleep-for 0 1))))
  (when (get-buffer org-sql-buffer)
    (kill-buffer org-sql-buffer)))

(defun org-sql--pragma-merge-default (&optional pragma)
  "Add PRAGMA to `org-sql--default-pragma'.
PRAGMA is a plist as described in `org-sql--fmt-pragma'. Return a
new plist with values from PRAGMA added, except for pragma already in
`org-sql--default-pragma'."
  (if (not pragma) org-sql--default-pragma
    (let ((getv
           (lambda (p)
             (or (plist-get org-sql--default-pragma p)
                 (plist-get pragma p)))))
      (->>
       org-sql--default-pragma
       org-sql--plist-get-keys
       (append (org-sql--plist-get-keys pragma))
       delete-dups
       (mapcan (lambda (p) `(,p ,(funcall getv p))))))))

(defun org-sql--cmd-set-pragma (&optional pragma)
  "Set the pragma of the running SQL connection.
PRAGMA is a plist of pragma to set. This is merged with
`org-sql-default-pragma' before executing in `org-sql-buffer'."
  (->> pragma
       org-sql--pragma-merge-default
       org-sql--fmt-pragma
       org-sql-cmd))

;; new sql-redirect function with windows prompt fixed
;; use advice and system-type = 'windows-nt' varable to wrap the
;; advice
  
(defun org-sql-cmd (cmd)
  "Execute SQL string CMD in SQLi buffer named as `org-sql-buffer'.
If buffer process not running, it is started automatically. Returns
the output of CMD as given by the running SQL shell."
  (when cmd
    (org-sql--with-advice
        ;; this function will throw a "regex too long error"
        ((#'looking-at :override #'ignore))
      ;; TODO add a debug option here so the temp buffer is not
      ;; thrown away
      (let ((temp-buf "*SQL: Out*")
            (get-output
             (lambda (b)
               (with-current-buffer b
                 (let ((out (buffer-substring-no-properties
                             (point-min)
                             (point-max))))
                   (kill-buffer b)
                   out)))))
        (sql-redirect-one org-sql-buffer cmd temp-buf nil)
        (->> temp-buf (funcall get-output) string-trim)))))

(defun org-sql-cmd-select (tbl-name &optional cols conds)
  "Select columns from TBL-NAME where COLS is the list of columns.
If COLS is nil, all columns will be returned. Columns is expected as
a list of keywords like ':col1' and ':col2'. CONDS, if supplied, is
a plist of conditions to test in the select statement. (currently
joined by AND)"
  (let* ((colnames
          (if (not cols) "*"
            (--> cols
                 (mapcar #'org-sql--kw-to-colname it)
                 (string-join it ","))))
         (tbl-str (symbol-name tbl-name))
         (cmd (if (not conds)
                  (format "select %s from %s;" colnames tbl-str)
                (--> conds
                     (org-sql--plist-concat it " and ")
                     (format "select %s from %s where %s;" colnames
                             tbl-str it)))))
    (--> cmd (org-sql-cmd it) (org-sql--to-plist it cols))))

;;; org-mode string parsing functions

(defun org-sql--effort-to-int (effort-str)
  "Convert EFFORT-STR into an integer from HH:MM format.
If it is already an integer, nothing is changed. If TO-STRING is t,
convert the final number to a string of the number. If THROW-ERR is t,
throw an error if the string is not recognized."
  (pcase (-some->> effort-str
           (string-trim)
           (s-match "^\\(\\([0-9]+\\)\\|\\([0-9]+\\):\\([0-6][0-9]\\)\\)$")
           (-drop 2))
    (`(nil ,h ,m) (+ (* 60 (string-to-number h)) (string-to-number m)))
    (`(,m) (string-to-number m))))

;;; org-mode element helper functions
        
(defun org-sql--headline-get-path (headline)
  "Return the path for HEADLINE node.

Return a string formatted as /level1/level2/.../levelN for each
level in HEADLINE's path (not including the current headline)."
  (->> (org-ml-headline-get-path headline)
       (-drop-last 1)
       (s-join "/")
       (format "/%s")))
        
(defun org-sql--headline-get-archive-itags (headline)
  "Return archive itags from HEADLINE or nil if none."
  (when org-sql-use-tag-inheritance
    (-some-> (org-ml-headline-get-node-property "ARCHIVE_ITAGS" headline)
      (split-string))))

(defun org-sql--headline-get-tags (headline)
  "Return list of tags from HEADLINE."
  (->> (org-ml-get-property :tags headline)
       (-map #'substring-no-properties)))

(defun org-sql--element-parent-tags (acc headline)
  "Get all tags from parent headlines of HEADLINE.
Add tags to ACC (which is treated like a set)."
  (cl-labels
      ((get-tags
        (acc hl)
        (if (eq (car hl) 'org-data) acc
          (-> (org-sql--headline-get-archive-itags hl)
              (-union (org-sql--headline-get-tags hl))
              (-union acc)
              (get-tags (org-ml-get-property :parent hl))))))
    (get-tags acc (org-ml-get-property :parent headline))))

(defun org-sql--todo-keywords ()
 "Return `org-todo-keywords' as list of strings w/o selectors.
Will likely match the value of `org-todo-keywords-1' in many cases,
but this has the advantage of being always available and
comprehensive."
 (->> org-todo-keywords
      copy-tree
      (mapcan #'cdr)
      (remove "|")
      (--map (replace-regexp-in-string "(.*)" "" it))))

(defun org-sql--regexp-remove-captures (regexp)
  "Return REGEXP string with captures removed."
  (s-replace-all '(("\\(" . "") ("\\)" . "")) regexp))

(defun org-sql--log-note-headings-convert ()
  "Convert `org-log-note-headings' to a regex matcher.
This is used to set `org-sql--log-note-headings-regexp'; see this
constant for further details."
  (cl-labels
      ((format-capture
        (regexp)
        (->> (s-replace-all '(("\\(" . "") ("\\)" . "")) regexp)
             (format "\\(%s\\)")))
       (reverse-lookup
        (value alist)
        (car (--find (equal (cdr it) value) alist))))
    (let* ((keys '((:user .  "%u")
                   (:user-full . "%U")
                   (:ts . "%t")
                   (:ts-active . "%T")
                   (:short-ts . "%d")
                   (:short-ts-active . "%D")
                   (:old-state . "%S")
                   (:new-state . "%s")))
           (ts-or-todo-regexp (->> (org-sql--todo-keywords)
                                   (-map #'regexp-quote)
                                   (cons org-ts-regexp-inactive)
                                   (s-join "\\|")
                                   (format-capture)
                                   (format "\"%s\"")))
           (ts-regexp (format-capture org-ts-regexp))
           (ts-ia-regexp (format-capture org-ts-regexp-inactive))
           (re-match-alist
            (->> (list "\\(.*\\)"
                       "\\(.*\\)"
                       ts-ia-regexp
                       ts-regexp
                       ts-ia-regexp
                       ts-regexp
                       ts-or-todo-regexp
                       ts-or-todo-regexp)
                 (--map (concat "[[:space:]]*" it "[[:space:]]*"))
                 (-zip-pair (-map #'cdr keys))))
           (unpadded-headings
            (->> (-map #'cdr org-log-note-headings)
                 (--map (org-replace-escapes it (->> (-map #'cdr keys)
                                                     (--map (cons it it)))))))
           (heading-types (-map #'car org-log-note-headings))
           (heading-regexps (->> unpadded-headings
                                 (--map (s-replace-regexp "\s+" " " it))
                                 (--map (org-replace-escapes it re-match-alist))))
           (heading-keys (->> unpadded-headings
                              (--map (s-match-strings-all "%[[:alpha:]]" it))
                              (--map (-map #'car it))
                              (--map (--map (reverse-lookup it keys) it)))))
      (->> (-zip-lists heading-types heading-regexps heading-keys)
           (--remove (equal (cadr it) ""))))))
           
(defconst org-sql--log-note-headings-regexp
  (org-sql--log-note-headings-convert)
  "Like `org-log-note-headings' with regexps.
Each regexp matches the text that will be inserted into the
escape sequences of `org-log-note-headings'.")

;; (clock :offset :note-text :state-old :state-new)
;; (state (:offset :note-text :header-text :state-old :state-new :ts)
;; ((re/del)/(schedule/deadline) (:offset :note-text :header-text :state-old :ts)
;; (refile/done/note (:offset :note-text :header-text :ts)
;; (none (:offset :note-text :header-text)

(defun org-sql--lb-match-header (header-text)
  "Match HEADER-TEXT with `org-sql--log-note-headings-regexp'.
If match successful, returns list whose car is the match type
and cdr is the match data."
  ;; ASSUME all keys are unique (this will crash and burn if not true)
  (cl-labels
      ((match-sum
        (regexp i)
        (s-matched-positions-all regexp header-text i))
       (match-header
        (acc cell)
        (if acc acc
          (-let (((type regexp keys) cell))
            (-some->> keys
              (--map-indexed (cons it (match-sum regexp (1+ it-index))))
              (--filter (cdr it))
              (apply #'append)
              (cons type))))))
    (or (->> org-sql--log-note-headings-regexp
             (-reduce-from #'match-header nil))
        '(none))))

;; TODO this could be included in org-ml
(defun org-sql--split-paragraph (paragraph)
  "Split PARAGRAPH by first line-break node."
  (let ((children (org-ml-get-children paragraph)))
    (-if-let (lb-index (--find-index (org-ml-is-type 'line-break it) children))
        (-let* (((head _rest) (-split-at lb-index children))
                ((break . rest) _rest)
                ;; assume begin/end should be the same as contents-begin/end
                (parent (org-ml-get-property :parent (-first-item head)))
                (b1 (org-ml-get-property :begin parent))
                (e1 (org-ml-get-property :begin break))
                (b2 (org-ml-get-property :end break))
                (e2 (org-ml-get-property :end parent))
                (head* (->> (apply #'org-ml-build-paragraph head)
                            (org-ml--set-properties-nocheck
                             (list :begin b1
                                   :contents-begin b1
                                   :end e1
                                   :contents-end e1))))
                (rest* (-some->> rest
                         (apply #'org-ml-build-paragraph)
                         (org-ml--set-properties-nocheck
                          (list :begin b2
                                :contents-begin b2
                                :end e2
                                :contents-end e2)))))
          (if (not rest*) `(nil . ,head*) `(,head* . ,rest*)))
      `(nil . ,paragraph))))

;; TODO this could be included in org-ml
(defun org-sql--item-get-contents (item)
  "Return the children of ITEM that are not items."
  (->> (org-ml-get-children item)
       (--take-while (not (org-ml-is-type 'plain-list it)))))

;; TODO this could be included in org-ml
(defun org-sql--split-item (item)
  "Split the contents of ITEM by the first line break."
  (-let (((first . rest) (org-sql--item-get-contents item)))
    (when first
      (if (not (org-ml-is-type 'paragraph first)) (cons nil contents)
        (-let (((p0 . p1) (org-sql--split-paragraph first)))
          (if (not p0) `(,p1 . ,rest) `(,p0 . (,p1 . ,rest))))))))

(defun org-sql--get-header-substring (entry key)
  (-let* ((e (cdr entry))
          ((&plist :header-text) e)
          ((begin . end) (plist-get e key)))
    (substring header-text begin end)))

(defun org-sql--get-header-timestamp (entry key)
  (-let* ((e (cdr entry))
          ((&plist :header-node) e)
          (header-begin (org-ml-get-property :begin header-node))
          (ts-offset (car (plist-get e key)))
          (ts-begin (+ header-begin ts-offset)))
    (->> (org-ml-get-children header-node)
         (--find (org-ml--property-is-eq :begin ts-begin it)))))

(defun org-sql--partition-item (item)
  "Partition org-element ITEM into plist."
  (-let* (((header . rest) (org-sql--split-item item))
          (header-text (org-ml-to-trimmed-string header))
          (note-text (-some->> (-map #'org-ml-to-string rest)
                       (s-join "")
                       (s-trim)))
          (header-data (org-sql--lb-match-header header-text)))
    (append header-data (list :header-node header
                              :header-text header-text
                              :note-text note-text
                              :offset (org-ml-get-property :begin item)))))

(defun org-sql--partition-clock (clock)
  "Partition CLOCK into typed plist."
  (let ((ts (org-ml-get-property :value clock)))
    (list 'clock
          :offset (org-ml-get-property :begin clock)
          :state-old (-> (org-ml-timestamp-get-start-time ts)
                         (org-ml-build-timestamp!))
          :state-new (-some-> (org-ml-timestamp-get-end-time ts)
                       (org-ml-build-timestamp!))
          :note-text nil)))

(defun org-sql--flatten-lb-entries (children)
  "Return logbook drawer CHILDREN as flattened list."
  (cl-labels
       ((add-node
         (clock-plist note)
         (cons (car clock-entry) (plist-put :note note (cdr clock-entry))))
        (merge-clock-notes
         (acc next)
         ;; if next node to add is a clock, partition and add it
         (if (org-ml-is-type 'clock next)
             (cons (org-sql--partition-clock next) acc)
           ;; else assume next node is a plain-list, partition its items
           (let* ((item-entries (->> (org-ml-get-children next)
                                     (-map #'org-sql--partition-item)))
                  (first-entry (car item-entries))
                  (other-entries (cdr item-entries))
                  (last (car acc)))
             ;; if the top item doesn't have a type, assume it is a clock note
             (if (and (eq (car last) 'clock) (eq (car first-entry) 'none))
                 (->> (cdr acc)
                      (cons (add-note last first-entry))
                      (append (reverse other-entries)))
               ;; else just append all the partitioned items
               (append (reverse item-entries) acc))))))
    (->> (--filter (org-ml-is-any-type '(clock plain-list) it) children)
         (-reduce-from #'merge-clock-notes nil)
         (reverse))))

;;; org element extraction functions
;;
;; These are functions used to pull data from the org-data tree
;; given by `org-element-parse-buffer'. They all adhere to the same
;; idiom where they take an accumulator as the first argument and
;; return a modified accumulator with the data to be added to the
;; database. The accumulator is an alist of plists that represents
;; the data to be inserted:
;; ((TABLE1 ((:COL1 VAL1 :COL2 VOL2) ..))
;;  (TABLE2 ((:COL1 VAL1 :COL2 VOL2) ..) ..))
;; where TABLEX is the table name, COLY is a column within TABLEX
;; and VALY is the value to add to COLY within TABLEX. Note that
;; COLY is supplied as a keyword where ':column-name' represents
;; 'column_name' in the database.

(defun org-sql--alist-put (alist prop value)
  "For given ALIST, append VALUE to the current values in prop.
Current values (that is the cdr of each key) is assumed to be a list.
If PROP does not exist, create it. Return the new alist."
  ;; NOTE: this function destructively modifies `alist'; this is fine so long as
  ;; the only thing we are doing to `alist' is adding to it
  (let* ((cur-cell (assoc prop alist))
         (cur-values (cdr cur-cell)))
      (cond
       (cur-values
        (setcdr cur-cell (cons value cur-values))
        alist)
       (cur-cell
        (setcdr cur-cell `(,value))
        alist)
       (alist
        (cons `(,prop ,value) alist))
       (t
        `((,prop ,value))))))

(defun org-sql--extract (acc fun objs &rest args)
  "Iterate through OBJS and add them to accumulator ACC with FUN.
FUN is a function that takes a single object from OBJS, the accumulator,
and ARGS. FUN adds OBJ to ACC and returns new ACC."
  (--reduce-from (apply fun acc it args) acc objs))

(defun org-sql--extract-lb-clock (acc entry headline fp)
  "Add data from logbook CLOCK to accumulator ACC."
  (if (not org-sql-store-clocks) acc
    (-let* (((&plist :offset :note-text) (cdr entry))
            ((&plist :state-old start :state-new end) (cdr entry))
           (clock-data
            (list :file_path fp
                  :headline_offset (org-element-property :begin headline)
                  :clock_offset offset
                  :time_start (-> (org-ml-timestamp-get-start-time start)
                                  (org-ml-time-to-unixtime))
                  :time_end (-some-> end
                              (org-ml-timestamp-get-start-time)
                              (org-ml-time-to-unixtime))
                  :clock_note (when org-sql-store-clock-notes note-text))))
      (org-sql--alist-put acc 'clocking clock-data))))

(defun org-sql--extract-lb-item (acc entry headline fp)
  "Add general logbook ENTRY to ACC."
  (-let* (((entry-type . entry-plist) entry)
          ((&plist :offset :header-text :note-text) entry-plist)
          (logbook-data
           (list :file_path fp
                 :headline_offset (org-ml-get-property :begin headline)
                 :entry_offset offset
                 :entry_type entry-type
                 :time_logged (-some->> (org-sql--get-header-timestamp entry :ts)
                                (org-ml-timestamp-get-start-time)
                                (org-ml-time-to-unixtime))
                 :header header-text
                 :note note-text)))
    (org-sql--alist-put acc 'logbook logbook-data)))

(defun org-sql--extract-lb-state-change (acc entry headline fp)
  "Add data from state-change logbook entry to accumulator ACC."
  (-let* (((&plist :offset) (cdr entry))
          (state-data
           (list :file_path fp
                 :entry_offset offset
                 :state_old (org-sql--get-header-substring entry :old-state)
                 :state_new (org-sql--get-header-substring entry :new-state))))
    (-> (org-sql--extract-lb-item acc entry headline fp)
        (org-sql--alist-put 'state_changes state-data))))

(defun org-sql--extract-lb-planning-change (acc entry headline fp)
  "Add data from planning-change logbook entry to accumulator ACC."
  (-let* (((&plist :offset) (cdr entry))
          (ts (org-sql--get-header-timestamp entry :old-state))
          (planning-data
           (list :file_path fp
                 :entry_offset offset
                 :timestamp_offset (org-ml-get-property :begin ts))))
    (-> (org-sql--extract-lb-item acc entry headline fp)
        (org-sql--alist-put 'planning_changes planning-data)
        (org-sql--extract-ts ts headline fp))))
         
(defun org-sql--extract-logbook (acc headline fp)
  "Given HL-PART, find logbook drawer and add to accumulator ACC."
  (cl-flet
      ((extract-entry
        (acc entry)
        (let ((entry-type (car entry)))
          ;; TODO add clock to default logbook entries
          (if (not (or (eq entry-type 'clock)
                       (memq entry-type org-sql-included-logbook-types)))
              acc
            (cl-case entry-type
              ((redeadline deldeadline reschedule delschedule)
               (org-sql--extract-lb-planning-change acc entry headline fp))
              (state
               (org-sql--extract-lb-state-change acc entry headline fp))
              (clock
               (org-sql--extract-lb-clock acc entry headline fp))
              (t
               (org-sql--extract-lb-item acc entry headline fp)))))))
    (->> (org-ml-headline-get-logbook headline)
         (org-sql--flatten-lb-entries)
         (-reduce-from #'extract-entry acc))))

(defun org-sql--extract-properties (acc headline fp)
  "Add properties data from HL-PART and add to accumulator ACC."
  (if (eq 'all org-sql-ignored-properties) acc
    (let ((node-props
           (->> (org-ml-headline-get-node-properties headline)
                (--remove (member (org-ml-get-property :key it)
                                  (append org-sql--ignored-properties-default
                                          org-sql-ignored-properties))))))
      (cl-flet
          ((from
            (acc np)
            (->> (list :file_path fp
                       :headline_offset (org-ml-get-property :begin headline)
                       :property_offset (org-ml-get-property :begin np)
                       :key_text (org-ml-get-property :key np)
                       :val_text (org-ml-get-property :value np)
                       ;; TODO add inherited flag
                       :inherited nil)
                 (org-sql--alist-put acc 'properties))))
        (org-sql--extract acc #'from node-props)))))

(defun org-sql--extract-tags (acc headline fp)
  "Extract tags data from HEADLINE and add to accumulator ACC."
  (if (eq 'all org-sql-ignored-tags) acc
    (cl-flet
        ((from
          (acc tag inherited)
          (->> (list :file_path fp
                     :headline_offset (org-ml-get-property :begin headline)
                     :tag tag
                     :inherited inherited)
               (org-sql--alist-put acc 'tags)))
         (filter-ignored
          (tags)
          (-difference tags org-sql-ignored-tags)))
      (let ((tags (filter-ignored (org-sql--headline-get-tags headline)))
            (i-tags (--> (org-sql--headline-get-archive-itags headline)
                         (if (not org-sql-use-tag-inheritance) it
                           (org-sql--element-parent-tags it headline))
                         (filter-ignored it))))
        (-> (org-sql--extract acc #'from tags nil)
            (org-sql--extract #'from i-tags t))))))

(defun org-sql--extract-links (acc headline fp)
  "Add link data from headline HEADLINE to accumulator ACC."
  (if (eq 'all org-sql-ignored-link-types) acc
    (let ((links (->> (org-ml-match '(:any * link) headline)
                      (--remove (member (org-ml-get-property :type it)
                                        org-sql-ignored-link-types)))))
      (cl-flet
          ((from
            (acc link)
            (->> (list :file_path fp
                       :headline_offset (org-ml-get-property :begin headline)
                       :link_offset (org-ml-get-property :begin link)
                       :link_path (org-ml-get-property :path link)
                       :link_text (->> (org-ml-get-children link)
                                       (-map #'org-ml-to-string)
                                       (s-join ""))
                       :link_type (org-ml-get-property :type link))
                 (org-sql--alist-put acc 'links))))
        (org-sql--extract acc #'from links)))))

(defun org-sql--extract-ts (acc ts headline fp)
  "Add timestamp TS data from headline HL-PART to accumulator ACC.
PT is a string representing the planning type and is one of 'closed,'
'scheduled,' or 'deadline' although these values are not enforced by
this function."
  (cl-flet
      ((get-resolution
        (time)
        ;; TODO this should be public in org-ml
        (when time
          (if (org-ml--time-is-long time) 'minute 'day))))
    (let* ((start (org-ml-timestamp-get-start-time ts))
           (end (org-ml-timestamp-get-end-time ts))
           (ts-data
            (list :file_path fp
                  :headline_offset (org-ml-get-property :begin headline)
                  :timestamp_offset (org-ml-get-property :begin ts)
                  :type (if (org-ml-timestamp-is-active ts) 'active 'inactive)
                  ;; :planning_type planning-type
                  :planning_type nil
                  :warning_type (org-ml-get-property :warning-type ts)
                  :warning_value (org-ml-get-property :warning-value ts)
                  :warning_unit (org-ml-get-property :warning-unit ts)
                  :repeat_type (org-ml-get-property :repeater-type ts)
                  :repeat_value (org-ml-get-property :repeater-value ts)
                  :repeat_unit (org-ml-get-property :repeater-unit ts)
                  :time (org-ml-time-to-unixtime start)
                  :resolution (get-resolution start)
                  :time_end (-some-> end (org-ml-time-to-unixtime))
                  :resolution_end (get-resolution end)
                  :raw_value (org-ml-get-property :raw-value ts))))
      (org-sql--alist-put acc 'timestamp ts-data))))

(defun org-sql--extract-hl-contents (acc headline fp)
  "Add contents from partitioned header HEADLINE to accumulator ACC."
  ;; TODO this only works when `org-log-into-drawer' is defined
  (let ((timestamps
         (->> (org-ml-headline-get-section headline)
              ;; TODO need a function in org-ml that returns non-meta
              (--remove (org-ml-is-any-type '(planning property-drawer) it))
              (--remove (equal (org-element-property :drawer-name it)
                               org-log-into-drawer))
              (org-ml-match '(:any * timestamp))
              (--filter (org-ml-is-any-type org-sql-included-contents-timestamp-types it)))))
    (org-sql--extract acc #'org-sql--extract-ts timestamps headline fp)))

(defun org-sql--extract-hl-planning (acc headline fp)
  "Add planning timestamps from HEADLINE to accumulator ACC.
This will include planning timestamps according to
`org-sql-included-headline-planning-types'."
  ;; TODO make offset entries in headline table for these
  (-if-let (planning (org-ml-headline-get-planning headline))
      (->> org-sql-included-headline-planning-types
           (--map (org-ml-get-property it planning))
           (--remove (null (cdr it)))
           (--reduce-from (org-sql--extract-ts acc it headline fp) acc))
    acc))

(defun org-sql--extract-hl-meta (acc headline fp)
  "Add general data from HEADLINE to accumulator ACC."
  (let ((hl-data
         (list
          :file_path fp
          :headline_offset (org-ml-get-property :begin headline)
          :tree_path (org-sql--headline-get-path headline)
          :headline_text (org-ml-get-property :raw-value headline)
          :keyword (org-ml-get-property :todo-keyword headline)
          :effort (-some-> (org-ml-headline-get-node-property "Effort" headline)
                    (org-sql--effort-to-int))
          :priority (-some->> (org-ml-get-property :priority headline)
                      (byte-to-string))
          :archived (org-ml-get-property :archivedp headline)
          :commented (org-ml-get-property :commentedp headline)
          :content nil)))
    (-> (org-sql--alist-put acc 'headlines hl-data)
        (org-sql--extract-hl-contents headline fp)
        (org-sql--extract-hl-planning headline fp))))

(defun org-sql--extract-hl (acc headlines fp)
  "Extract data from HEADLINES and add to accumulator ACC.
FP is the path to the file containing the headlines."
  (cl-flet
      ((from
        (acc hl)
        (-> (org-sql--extract-hl-meta acc hl fp)
            (org-sql--extract-links hl fp)
            (org-sql--extract-tags hl fp)
            (org-sql--extract-properties hl fp)
            (org-sql--extract-logbook hl fp)
            (org-sql--extract-hl (org-ml-headline-get-subheadlines hl) fp))))
    (org-sql--extract acc #'from headlines)))

(defun org-sql--extract-buffer (acc fp)
  "Extracts all headlines from the current buffer to ACC.
FP is the filepath where the buffer lives."
  (let ((headlines (--> (org-element-parse-buffer)
                        (org-element-contents it)
                        (if (assoc 'section it) (cdr it) it))))
    (org-sql--extract-hl acc headlines fp)))

(defun org-sql--extract-file (cell acc)
  "Extract the file in the car of CELL for a sql insertion.
The results are accumulated in ACC which is returned on exit."
  (let* ((fp (car cell))
         (md5sum (cdr cell))
         (fsize (->> fp file-attributes file-attribute-size))
         (file-data (list :file_path fp :md5 md5sum :size fsize)))
    (with-current-buffer (find-file-noselect fp t)
      (-> acc
          (org-sql--alist-put 'files file-data)
          (org-sql--extract-buffer fp)))))

;;; database syncing functions

(defun org-sql-sync-insert (cell acc)
  "Add insertion commands for CELL in accumulator ACC. Return new ACC."
  (->> (plist-get acc 'insert)
       (org-sql--extract-file cell)
       (plist-put acc 'insert)))

(defun org-sql-sync-update (cell acc)
  "Add update commands for CELL in accumulator ACC. Return new ACC."
  (let ((updt-acc (plist-get acc 'update)))
    (->> `((:file_path ,(car cell)) . (:md5 ,(cdr cell)))
         (org-sql--alist-put updt-acc 'files)
         (plist-put acc 'update))))

(defun org-sql-sync-delete (cell acc)
  "Add deletion commands for CELL in accumulator ACC. Return new ACC."
  (let ((dlt-acc (plist-get acc 'delete)))
    (->>  `(:file_path ,(car cell))
          (org-sql--alist-put dlt-acc 'files)
          (plist-put acc 'delete))))

;; TODO can probs rewrite this in a clearer way using partitioning
;; from dash
(defun org-sql-sync-one (cell fp-qry acc)
  "Match CELL with entries FP-QRY and process accordingly.
CELL is a cons cell given by `org-sql-files-on-disk' and FP-QRY
is a list of cons cells given by `org-sql-files-from-db'.

By comparing the file path and md5 in CELL with those contained in
FP-QRY, this function will determine the sync state between disk and
db for the file represented by CELL. These scenarios can occur:

- both filepath and md5 match: do nothing, fully synced

- filepath doesn't match: assume the file was renamed and update db
  with filepath from CELL

- md5 doesn't match: assume file was modified; delete the path from
  the db and repopulate the filepath from CELL

- neither match: assume file is new and untracked; insert filepath
  from CELL into db

Returns a cons cell of the new accumulator ACC and the remaining
FP-QRY. If a match is found is it removed fro FP-QRY before returning.

Note that this does not test if there are entries in the db that
have no files on disk. This is dealt with in `org-sql-sync'."
  ;; if perfect match, do nothing
  (if (cl-find cell fp-qry :test #'equal)
      (cons acc (remove cell fp-qry))
    (let* ((match-cells
            (lambda (a b fun)
              (let ((car-a (car a))
                    (cdr-a (cdr a))
                    (car-b (car b))
                    (cdr-b (cdr b)))
                (funcall fun car-a car-b cdr-a cdr-b))))
           (match-fp
            (lambda (fp-a fp-b md5-a md5-b)
              (and (equal fp-a fp-b) (not (equal md5-a md5-b)))))
           (match-md5
            (lambda (fp-a fp-b md5-a md5-b)
              (and (not (equal fp-a fp-b)) (equal md5-a md5-b))))
           (match-fp*
            (lambda (b)
              (funcall match-cells cell b match-fp)))
           (match-md5*
            (lambda (b)
              (funcall match-cells cell b match-md5)))
           (found-fp (cl-find-if (lambda (q) (funcall match-fp* q)) fp-qry)))
      (cond
       ;; if fp matches, delete qry in db and insert cell
       (found-fp
        (cons (org-sql-sync-insert cell (org-sql-sync-delete found-fp acc))
              (remove found-fp fp-qry)))
       ;; if md5 matches, update fp in db
       ((cl-find-if (lambda (q) (funcall match-md5* q)) fp-qry)
        (cons (org-sql-sync-update cell acc)
              (cl-remove-if (lambda (q) (funcall match-md5* q)) fp-qry)))
       ;; if none match, insert cell
       (t
        (cons (org-sql-sync-insert cell acc) fp-qry))))))

(defun org-sql-sync-all (fp-dsk fp-qry)
  "Synchronize state between disk and db.

FP-DSK and FP-QRY are lists of cons cells as returned via
`org-sql-files-in-disk' and `org-sql-files-in-db' respectively.
This function iterates through all cells in FP-QRY, interrogating
their sync state via `org-sql-sync-one' (this takes care of any
insertion and update operations for cells in FP-DSK). Anything in
FP-QRY that is not matched with anything in FP-DSK is assumed to be
deleted and is removed at the end of this function.

This creates and returns an accumulator object which is an alist of
alists of plists which holds the operations to be performed on the
database."
  (let (acc)
    ;; sync each cell in fp-dsk first and remove matching fp-qry cells
    (while fp-dsk
      (let ((found (--> fp-dsk
                        (car it)
                        (org-sql-sync-one it fp-qry acc))))
        (setq fp-dsk (cdr fp-dsk)
              acc (car found)
              fp-qry (cdr found))))
    ;; remove all leftover entries in the db
    (while fp-qry
      (setq acc (org-sql-sync-delete (car fp-qry) acc)
            fp-qry (cdr fp-qry)))
    acc))

(defun org-sql-files ()
  "Return full list of absolute file paths via `org-sql-files'."
  (->>
   org-sql-files
   (--map (if (file-directory-p it)
              (directory-files it t "\\`.*\\.org\\(_archive\\)?\\'")
            (list it)))
   (apply #'append)
   (-filter #'file-exists-p)))

(defun org-sql-files-on-disk ()
  "Return alist for file paths in `org-sql-files'.
In each cell, the car is the file path and cdr is the file's MD5."
  (let ((cons-md5
         (lambda (fp)
           (->> fp
                (format "md5sum %s | awk '{print $1}'")
                shell-command-to-string
                string-trim
                (cons fp)))))
    (->> (org-sql-files) (--map (funcall cons-md5 it)))))

(defun org-sql-files-in-db ()
  "Get all files and their metadata from the database.
Returns an alist where the each car is the file_path column value
and each cdr is the plist of metadata."
  (when (file-exists-p org-sql-sqlite-path)
    (->> '(:file_path :md5)
         (org-sql-cmd-select 'files)
         (mapcar #'org-sql--plist-get-vals)
         (--map (cons (car it) (cadr it))))))

(defun org-sql-get-transactions (&optional newlines)
  "Return plist of the transactions to be performed on the db.
The plist has three properties (delete, insert, update) for the three
type of commands that are performed on the database during an update.
If NEWLINES is t, add newlines between SQL commands; this is useful
for dumping to buffers."
  (let ((fp-dsk (org-sql-files-on-disk))
        (map-trns
         (lambda (op fun trans)
           (-->
            (plist-get trans op)
            (--map (funcall fun it) it)
            (org-sql--fmt-trans it newlines)
            (plist-put trans op it)))))
    (->>
     (org-sql-files-in-db)
     (org-sql-sync-all fp-dsk)
     (funcall map-trns 'insert #'org-sql--fmt-inserts)
     (funcall map-trns 'update #'org-sql--fmt-updates)
     (funcall map-trns 'delete #'org-sql--fmt-deletes))))

(defun org-sql-dump-update-transactions ()
  "Dump the transactions to be committed the database during an update.

It will have three sections denoted \"### DELETE ###\", \" ###
UPDATE ###\", and \"### INSERT ###\". Note this function is only
useful for debugging where one wants to see the exact
transactions to be committed and/or save a file to run the SQL
commands outside of this package."
  (interactive)
  (let ((out (->> (org-sql-get-transactions t)
                  (-partition 2)
                  (--map (-as-> (car it)
                                header
                                (symbol-name header)
                                (upcase header)
                                (format "### %s ###\n\n%s"
                                        header (cadr it))))
                  (reverse))))
    (switch-to-buffer "SQL: Org-update-dump")
    (insert (string-join out "\n\n"))))

(defun org-sql-init-db ()
  "Add schemas to database if they do not exist already.
This assumes an active connection is open."
  ;; assume that the db will be created when a new connection is opened
  (->> org-sql--schemas (mapcar #'org-sql-cmd)))

(defun org-sql-delete-db ()
  "Deletes the database from disk."
  (when (file-exists-p org-sql-sqlite-path)
    (delete-file org-sql-sqlite-path)))

(defun org-sql-update-db ()
  "Update the database. This assumes an active connection is open."
  (let ((trans (org-sql-get-transactions)))
    ;; the order below likely doesn't matter if the pragma is set
    ;; to defer foreign key constraints
    `(,(-> trans (plist-get 'delete) (org-sql-cmd))
      ,(-> trans (plist-get 'update) (org-sql-cmd))
      ,(-> trans (plist-get 'insert) (org-sql-cmd)))))

(defun org-sql-clear-db ()
  "Clear the database. This assumes an active connections is open."
  ;; only delete from files as we assume actions here cascade down
  (-> (org-sql--fmt-delete 'files nil t)
      list
      (org-sql--fmt-trans)
      (org-sql-cmd)))

;;; interactive user functions

(defun org-sql-user-update ()
  "Update the Org SQL database."
  (interactive)
  ;; TODO need to see if schema is correct?
  ;; for now this assumes the db exists and has a valid schema
  (org-sql-cmd-open-connection)
  (message "Updating Org SQL database")
  (let ((out (org-sql-update-db)))
    (when org-sql-debug
      (message "Debug output for org-sql update")
      ;; assume `OUT' is a list of the output for the three
      ;; transactions used in the update (delete, update, insert)
      (--> (--map (cond
                   ((null it) "Not run")
                   ((equal it "") "Run successfully")
                   (t it))
                  out)
           (-zip-pair '("DELETE" "UPDATE" "INSERT") it)
           (--map (format "%s transactions: %s" (car it) (cdr it)) it)
           (-each it #'message))))
  (message "Org SQL update complete"))

(defun org-sql-user-clear-all ()
  "Remove all entries in the database."
  (interactive)
  (if (y-or-n-p "Really clear all? ")
      (progn
        (org-sql-cmd-open-connection)
        (message "Clearing Org SQL database")
        (let ((out (org-sql-clear-db)))
          (when org-sql-debug
            (message "Debug output for org-sql clear-all")
            ;; assume `OUT' is a blank string (success) or the error
            ;; message
            (let ((msg (if (equal out "") "Run Successfully" out)))
              (message "DELETE transaction: %s" msg))))
        (message "Org SQL clear completed"))
    (message "Aborted")))

(defun org-sql-user-reset ()
  "Reset the database with default schema."
  (interactive)
  (if (or (not (file-exists-p org-sql-sqlite-path))
          (y-or-n-p "Really reset database? "))
      (progn
        (org-sql-cmd-kill-connection)
        (org-sql-delete-db)
        (org-sql-cmd-open-connection)
        (message "Resetting Org SQL database")
        (let ((out (org-sql-init-db)))
          (when org-sql-debug
            ;; assume `OUT' is a list of the output of all the
            ;; CREATE TABLE transactions when making the schema
            (message "Debug output for org-sql reset")
            (--> (--map (if (equal it "") "Run successfully" it) out)
                 (--map (format "CREATE TABLE transaction: %s" it) it)
                 (-each it #'message))))
        (message "Org SQL reset completed"))
    (message "Aborted")))

(provide 'org-sql)
;;; org-sql.el ends here
