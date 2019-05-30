;;; org-sql.el --- Org-Mode SQL converter -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, data
;; Homepage: https://github.com/ndwarshuis/org-sql
;; Package-Requires: ((emacs "25") (dash "2.15"))
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
(require 'sql)
(require 'org)

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

;; TODO add a debug buffer
;; (defconst org-sql-debug-buffer "*SQL: Org-Debug*"
;;   "Name of the SQLi buffer connected to the database.")

;;; helper functions

(defun org-sql--strip-string (str)
  "Remove text properties and trim STR and return the result."
  (when str (string-trim (substring-no-properties str))))
  
(defun org-sql--alist-put (alist prop value)
  "For given ALIST, append VALUE to the current values in prop.
Current values (that is the cdr of each key) is assumed to be a list.
If PROP does not exist, create it. Return the new alist. If FRONT is
t, add to the front of current values list instead of the back."
  (let* ((cur-cell (assoc prop alist))
         (cur-values (cdr cur-cell)))
      (cond
       (cur-values
        (let ((new-cdr (cons value cur-values)))
        ;; (let ((new-cdr (if front
                           ;; `(,value ,@cur-values)
                         ;; `(,@cur-values ,value))))
          (setcdr cur-cell new-cdr) alist))
       (cur-cell
        (setcdr cur-cell `(,value)) alist)
       (alist
        (append alist `((,prop ,value))))
       (t
        `((,prop ,value))))))
        
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
  (cond ((stringp entry) (org-sql--escape-text entry))
        ((numberp entry) (number-to-string entry))
        (entry (-> entry symbol-name org-sql--escape-text))
        (t "NULL")))

;; TODO this name is too specific
(defun org-sql--kw-to-colname (kw)
  "Return string representation of KW for column in sql database."
  (--> kw (symbol-name it) (substring it 1)))

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

(defun org-sql--fmt-trans (sql-str)
  "Format SQL transactions string.
SQL-STR is a list of individual SQL commands to be included in the
transaction."
  (-some->> sql-str
            (-flatten)
            (reverse)
            (string-join)
            (format "begin transaction; %s commit;")
            ;; turn on deferred keys for all transactions
            (concat "pragma defer_foreign_keys=on;")))

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

(defun org-sql--effort-to-int (effort-str &optional to-string throw-err)
  "Convert EFFORT-STR into an integer from HH:MM format.
If it is already an integer, nothing is changed. If TO-STRING is t,
convert the final number to a string of the number. If THROW-ERR is t,
throw an error if the string is not recognized."
  (when effort-str
    (let ((effort-str (string-trim effort-str)))
      (save-match-data
        (cond
         ((string-match "^\\([0-9]+\\):\\([0-6][0-9]\\)$" effort-str)
          (let* ((hours (string-to-number (match-string 1 effort-str)))
                 (minutes (string-to-number (match-string 2 effort-str)))
                 (sum (+ (* 60 hours) minutes)))
            (if to-string (number-to-string sum) sum)))
         ((string-match-p "^[0-9]+$" effort-str)
          (if to-string effort-str (string-to-number effort-str)))
         (t (when throw-err
              (error (concat "Unknown effort format: '" effort-str "'")))))))))

(defun org-sql--ts-fmt-unix-time (ts)
  "Return org timestamp TS as unix time integer.
Return nil if TS is nil or if TS cannot be understood."
  (-some-->
   ts
   (save-match-data (org-2ft it))
   (when (> it 0) (+ (round it)))))

(defun org-sql--parse-ts-range (ts &optional fun)
  "Return 'start' or 'end' of timestamp TS.
Return value will be a cons cell like (START . END) where START is
the first half of TS and END is the ending half or nil if it does not
exist.

By default, 'start' and 'end' are returned as integers in epoch time.
If different return values, are desired, supply function FUN which
takes a timestamp element (from either the start or end if a range) as
its sole argument."
  (when ts
    (let* ((get-epoch-time
            (lambda (ts)
              (->> ts
                   (org-element-property :raw-value)
                   (org-sql--ts-fmt-unix-time))))
           (fun (or fun get-epoch-time))
           (split
            (lambda (ts &optional end)
              (--> ts
                   (org-timestamp-split-range it end)
                   (funcall fun it)))))
      (if (eq (org-element-property :type ts) 'inactive-range)
          (let ((start (funcall split ts))
                (end (funcall split ts t)))
            (cons start end))
        `(,(funcall split ts))))))

(defun org-sql--parse-ts-maybe (txt)
  "Convert TXT to ISO 8601 format if possible.
Returns formatted string or TXT if it is not a timestamp."
  ;; assume the iso parser to return nil on failure
  (-> txt org-sql--ts-fmt-unix-time (or txt)))

;;; org-mode element helper functions

;; (defun org-sql--element-ts-raw (prop obj &optional iso)
;;   "Return the raw-value of the timestamp PROP in OBJ if exists.
;; If ISO is t, return the timestamp in ISO 8601 format."
;;   (-some--> obj
;;             (org-element-property prop it)
;;             (org-element-property :raw-value it)
;;             (if iso (org-sql--ts-fmt-unix-time it) it)))
            
(defun org-sql--element-split-by-type (type contents &optional right)
  "Split sequence of org-elements by first instance of TYPE.
CONTENTS is a list of org-element objects. If RIGHT is t, return the
list to the right of first-encountered TYPE rather than the left."
  (if right
      (cdr (--drop-while (not (eq type (org-element-type it))) contents))
    (--take-while (not (eq type (org-element-type it))) contents)))
        
(defun org-sql--element-parent-headline (obj)
  "Return parent headline element (if any) of org-element OBJ."
  (when obj
    (let ((parent (org-element-property :parent obj)))
      (if (eq 'headline (org-element-type parent))
          parent
        (org-sql--element-parent-headline parent)))))
        
(defun org-sql--element-parent-tree-path (obj &optional acc)
  "Construct parent tree path for OBJ and concatenate to ACC.
Returns '/' delimited path of headlines or nil if obj is in a toplevel
headline."
  (let ((parent-hl (org-sql--element-parent-headline obj)))
    (if parent-hl
        (--> parent-hl
             (org-element-property :raw-value it)
             (concat "/" it acc)
             (org-sql--element-parent-tree-path parent-hl it))
      acc)))
      
(defun org-sql--element-parent-tags (obj &optional acc)
  "Get all tags from parent headlines of OBJ and concat to ACC.
ACC is treated as a set; therefore no duplicates are retained."
  (let ((parent-hl (org-sql--element-parent-headline obj)))
    (if parent-hl
        (let* ((tags (->>
                      parent-hl
                      (org-element-property :tags)
                      (mapcar #'org-sql--strip-string)))
               (i-tags (org-element-property :ARCHIVE_ITAGS parent-hl))
               (i-tags (when i-tags (split-string i-tags)))
               (all-tags (delete-dups (append acc tags i-tags))))
          (org-sql--element-parent-tags parent-hl all-tags))
      acc)))

(defun org-sql--todo-keywords ()
 "Return `org-todo-keywords' as list of strings w/o selectors.
Will likely match the value of `org-todo-keywords-1' in many cases,
but this has the advantage of being always available and
comprehensive."
 (->>
  org-todo-keywords
  copy-tree
  (mapcan #'cdr)
  (remove "|")
  (--map (replace-regexp-in-string "(.*)" "" it))))

(defun org-sql--log-note-headings-convert ()
  "Convert `org-log-note-headings' to a regex matcher.
This is used to set `org-sql--log-note-headings-regexp'; see this
constant for further details."
  (let* ((escapes '("%u" "%U" "%t" "%T" "%d" "%D" "%s" "%S"))
         (ts-or-todo-regexp
          (-->
           (org-sql--todo-keywords)
           (mapconcat #'regexp-quote it "\\|")
           (format "\"\\(%s\\|%s\\)\"" org-ts-regexp-inactive it)))
         (ts-regexp (format "\\(%s\\)" org-ts-regexp))
         (ts-ia-regexp (format "\\(%s\\)" org-ts-regexp-inactive))
         (re-no-pad-alist (-zip-pair escapes escapes))
         (re-match-alist
          (->>
           (list "\\(.*\\)"
                 "\\(.*\\)"
                 ts-ia-regexp
                 ts-regexp
                 ts-ia-regexp
                 ts-regexp
                 ts-or-todo-regexp
                 ts-or-todo-regexp)
           (--map (concat "[[:space:]]*" it "[[:space:]]*"))
           (-zip-pair escapes)))
         (apply2note
          (lambda (n f) (cons (car n) (funcall f (cdr n)))))
         (replace-esc
          (lambda (n re)
            (funcall apply2note
                     n
                     (lambda (s) (org-replace-escapes s re)))))
         (shrink-space
          (lambda (n)
            (funcall apply2note
                     n
                     (lambda (s) (replace-regexp-in-string "\s+" " " s))))))
    (->>
     org-log-note-headings
     ;; remove padding information by replacing all escape sequences
     ;; with their non-padded version and then removing extra spaces
     (--map (funcall replace-esc it re-no-pad-alist))
     (--map (funcall shrink-space it))
     ;; replace all escape sequences with regexps that match
     ;; the data to be inserted via the escape sequences
     (--map (funcall replace-esc it re-match-alist))
     ;; filter out anything that is blank (eg default clock-in)
     (seq-filter (lambda (s) (not (equal (cdr s) "")))))))

(defconst org-sql--log-note-headings-regexp
  (org-sql--log-note-headings-convert)
  "Like `org-log-note-headings' with regexps.
Each regexp matches the text that will be inserted into the
escape sequences of `org-log-note-headings'.")

(defun org-sql--lb-match-header (header-text)
  "Match HEADER-TEXT with `org-sql--log-note-headings-regexp'.
If match successful, returns list whose car is the match type
and cdr is the match data."
  (letrec ((scan
            (lambda (str note-regex-alist)
              (when note-regex-alist
                (let* ((cur (car note-regex-alist))
                       (rem (cdr note-regex-alist))
                       (type (car cur))
                       (re (cdr cur)))
                  (if (string-match re str)
                      type
                    (funcall scan str rem))))))
           (type (funcall scan header-text org-sql--log-note-headings-regexp)))
    (when type (cons type (match-data)))))

(defun org-sql--flatten-lb-entries (lb-contents)
  "Given LB-CONTENTS, return a flattened list of clocks and items.
LB-CONTENTS is assumed to contain only org-elements clock and
plain-list, although this is not enforced here."
  (->> lb-contents
       (--map (if (eq 'clock (org-element-type it)) (list it)
                (org-element-map it 'item #'identity)))
       (apply #'append)))

(defun org-sql--split-lb-entries (lb-contents)
  "Split logbook entry list into logbook and non-logbook halves.

LB-CONTENTS is assumed to have only clock and plain-list org elements
objects, and this function is meant to evaluate the case where this
list is not contained within a logbook drawer and thus may have
non-logbook content at the end if LB-CONTENTS end with a plain-list
element.

Non-logbook content is determined by trying to match each item in the
last plain-list element `org-log-note-headings'. If this last
plain-list element is preceded by a clock, the first item is ignored
as it may be a clock note. Note that anything that is not in the last
run of items that does not match will go into the database with parse
 errors, as these should not happen.

Returns a cons cell where the car is a list lb-entries and the
cdr is a list of that are not part of the logbook.

The lb-entries are flattened using `org-sql--flatten-lb-entries'."
  ;; if last element is a plain list, check for non-logbook items
  (if (eq 'plain-list (org-element-type (-last-item lb-contents)))
      (let* ((clock-present? (assoc 'clock lb-contents))
             (last-split (-->
                          lb-contents
                          (-last-item it)
                          (list it)
                          (org-sql--flatten-lb-entries it)))
             (butlast-split (-->
                             lb-contents
                             (-butlast it)
                             (org-sql--flatten-lb-entries it)
                             (if (not clock-present?) it
                               (append it `(,(-first-item last-split))))))
             (last-split-part (-->
                               last-split
                               (if clock-present? (-drop 1 it) it)
                               (--split-with
                                (--> it
                                     ;; use nil for hl-part here
                                     ;; because we only care about
                                     ;; the :type field
                                     (org-sql--partition-item it nil)
                                     (alist-get :type it))
                                it))))
        (cons (append butlast-split (car last-split-part))
              (cdr last-split-part)))
    (list (org-sql--flatten-lb-entries lb-contents))))

;;; org-mode element partitioning functions

(defun org-sql--partition-headline (headline fp)
  "Partition org-element HEADLINE into alist.

The alist will be structured as such:

:filepath - path to the headline's file as given by FP
:headline - original headline element
:section - the section contents of the headline if found
:subheadlines - list of subheadlines if any"
 (unless headline (error "No headline given"))
 (unless fp (error "No file path given"))
 (let* ((hl-contents (org-element-contents headline))
        (section (->> hl-contents (assoc 'section) org-element-contents))
        (subheadlines (if section (cdr hl-contents) hl-contents)))
   `((:headline . ,headline)
     (:filepath . ,fp)
     (:section . ,section)
     (:subheadlines . ,subheadlines))))

(defun org-sql--partition-item (item hl-part)
  "Partition org-element ITEM into alist.

ITEM is assumed to be part of a logbook. Return a alist with the
following structure:

:hl-part - the partitioned headline HL-PART surrounding the item,
  which is an object as described in `org-sql--partition-headline'
:item - the original item element
:header-text - the first line of the note which is standardized using
  `org-log-note-headings'
:note-text - the remainder of the note text as a trimmed string with
  no text properties (will be nil if item has no line-break element)
:type - the type of the item's header text (may be nil if unknown)
:match-data - match data associated with finding the type as done
  using `org-sql--log-note-headings-regexp' (may be nil if undetermined).

Anatomy of a logbook item (non-clocking):
- header-text with linebreak //
  note-text ... more text
- another header-text linebreak

The header text is solely used for determining :type and :match-data."
  (let* ((contents (->> item (assoc 'paragraph) org-element-contents))
         (header-text (->> contents
                           (org-sql--element-split-by-type 'line-break)
                           org-element-interpret-data
                           org-sql--strip-string))
         (note-text (--> contents
                         (org-sql--element-split-by-type 'line-break it t)
                         org-element-interpret-data
                         org-sql--strip-string))
         (header-match (org-sql--lb-match-header header-text)))
    `((:item . ,item)
      (:hl-part . ,hl-part)
      (:header-text . ,header-text)
      (:note-text . ,note-text)
      (:type . ,(car header-match))
      (:match-data . ,(cdr header-match)))))

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

(defun org-sql--extract (acc fun objs &rest args)
  "Iterate through OBJS and add them to accumulator ACC with FUN.
FUN is a function that takes a single object from OBJS, the accumulator,
and ARGS. FUN adds OBJ to ACC and returns new ACC."
  (while objs
    (setq acc (apply fun acc (car objs) args)
          objs (cdr objs)))
  acc)

(defun org-sql--extract-lb-planning-change (acc logbook-data item-part
                                                planning-type)
  "Add data from planning-change logbook entry to accumulator ACC.
ITEM-PART is a partitioned logbook item as described in
`org-sql--partition-item'. LOGBOOK-DATA is a plist as passed from
`org-sql--extract-lb-item' PLANNING-TYPE is the type of the timestamp
that was changed (either 'deadline' or 'scheduled')."
  (let* ((hl-part (alist-get :hl-part item-part))
         (ts-old (->> item-part
                      (alist-get :item)
                      (assoc 'paragraph)
                      (org-element-contents)
                      ;; assume old timestamp is always the first
                      (assoc 'timestamp)))
         (pc-data
          (list
           :file_path (plist-get logbook-data :file_path)
           :entry_offset (plist-get logbook-data :entry_offset)
           :timestamp_offset (org-element-property :begin ts-old))))
    (->
     acc
     (org-sql--alist-put 'logbook logbook-data)
     (org-sql--alist-put 'planning_changes pc-data)
     (org-sql--extract-ts ts-old hl-part planning-type))))

(defun org-sql--extract-lb-state-change (acc logbook-data item-part)
  "Add data from state-change logbook entry to accumulator ACC.
ITEM-PART is a partitioned logbook item as described in
`org-sql--partition-item'. LOGBOOK-DATA is a plist as passed from
`org-sql--extract-lb-item'"
  (save-match-data
    (set-match-data (alist-get :match-data item-part))
    (let* ((header-text (alist-get :header-text item-part))
           (state-data
            (list :file_path (plist-get logbook-data :file_path)
                  :entry_offset (plist-get logbook-data :entry_offset)
                  :state_old (match-string 3 header-text)
                  :state_new (match-string 1 header-text))))
      (-> acc
          (org-sql--alist-put 'logbook logbook-data)
          (org-sql--alist-put 'state_changes state-data)))))

(defun org-sql--item-time-logged (item-part)
  "Return time-logged of ITEM-PART or nil if it cannot be determined.
ITEM-PART is a partitioned item given by `org-sql--partition-item'."
  (let* ((type (alist-get :type item-part))
         (time-index
          (cond
           ((memq type '(done note refile)) 1)
           ((memq type '(reschedule delschedule redeadline deldeadline)) 3)
           ((eq type 'state) 5))))
    (when time-index
      (set-match-data (alist-get :match-data item-part))
      (->> item-part
           (alist-get :header-text)
           (match-string time-index)
           org-sql--ts-fmt-unix-time))))

(defun org-sql--extract-lb-item (acc item-part)
  "Add data from logbook entry ITEM-PART to accumulator ACC.
ITEM-PART is a partitioned logbook item as described in
`org-sql--partition-item'."
  (let* ((type (alist-get :type item-part))
         (hl-part (alist-get :hl-part item-part))
         (logbook-data
          (list :file_path (alist-get :filepath hl-part)
                :headline_offset (->> hl-part
                                      (alist-get :headline)
                                      (org-element-property :begin))
                :entry_offset (->> item-part
                                   (alist-get :item)
                                   (org-element-property :begin))
                :entry_type type
                :time_logged (org-sql--item-time-logged item-part)
                :header (alist-get :header-text item-part)
                :note (alist-get :note-text item-part))))
    (if (not (or (null type) (memq type org-sql-included-logbook-types)))
        acc
      (cond
       ((eq type 'state)
        (org-sql--extract-lb-state-change acc logbook-data item-part))

       ((memq type '(redeadline deldeadline))
        (org-sql--extract-lb-planning-change acc logbook-data item-part
                                             'deadline))

       ((memq type '(reschedule delschedule))
        (org-sql--extract-lb-planning-change acc logbook-data item-part
                                             'scheduled))

       ;; ((memq type '(done refile note))
       ;;  (org-sql--extract-lb-other acc logbook-data))

       ;; TODO, need a better way to handle unrecognized logbook items
       (t (org-sql--alist-put acc 'logbook logbook-data))))))

(defun org-sql--extract-lb-clock (acc clock hl-part &optional item)
  "Add data from logbook CLOCK to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'
and represents the headline surrounding the clock.
If ITEM is provided, check that this is a valid note that can be
added to the clock, else add it as a normal logbook entry."
  (let* ((item-part (org-sql--partition-item item hl-part))
         (item-type (alist-get :type item-part))
         (clock-data
          (lambda ()
            (let* ((hl (alist-get :headline hl-part))
                   (ts-range (->> clock
                                  (org-element-property :value)
                                  org-sql--parse-ts-range)))
              (list :file_path (alist-get :filepath hl-part)
                    :headline_offset (org-element-property :begin hl)
                    :clock_offset (org-element-property :begin clock)
                    :time_start (car ts-range)
                    :time_end (cdr ts-range))))))
    (cond
     ;; if item doesn't have type assume it is a clock note
     ((and org-sql-store-clocks item-part (not item-type)
           org-sql-store-clock-notes)
      (->> (list :clock_note (alist-get :header-text item-part))
           (append (funcall clock-data))
           (org-sql--alist-put acc 'clocking)))

     ;; but don't add a clock note if we don't want it
     ((and org-sql-store-clocks item-part (not item-type))
      (org-sql--alist-put acc 'clocking (funcall clock-data)))

     ;; if item has type then add it as a separate item with clock
     ((and org-sql-store-clocks item-part item-type)
      (-> acc
          (org-sql--alist-put 'clocking (funcall clock-data))
          (org-sql--extract-lb-item item-part)))

     ;; if no item just add the clock
     ((and org-sql-store-clocks (not item-part))
      (org-sql--alist-put acc 'clocking (funcall clock-data)))

     ;; if we don't want clocks but there is still an item,
     ;; add it as a logbook entry if it has a type
     ((and (not org-sql-store-clocks) item-part item-type)
      (org-sql--extract-lb-item acc item-part))

     ;; else return acc unaltered
     (t acc))))

(defun org-sql--extract-lb-items (acc items hl-part)
  "Add data from logbook ITEMS to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'
and represents the headline surrounding the items."
  (let ((from
         (lambda (acc item hl-part)
           (->> hl-part
                (org-sql--partition-item item)
                (org-sql--extract-lb-item acc)))))
    (org-sql--extract acc from items hl-part)))

(defun org-sql--extract-lb-one (acc entry hl-part)
  "Add data from logbook ENTRY to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'
and represents the headline surrounding the entry."
  (let ((type (org-element-type entry)))
    (cond
     ((eq type 'clock)
      (org-sql--extract-lb-clock acc entry hl-part))
     ((eq type 'item)
      (--> entry
           (org-sql--partition-item it hl-part)
           (org-sql--extract-lb-item acc it)))
     ;; TODO add an "UNKNOWN" logbook parser
     (t acc))))

(defun org-sql--extract-lb-contents (acc lb-flat hl-part)
  "Add list of logbook items LB-FLAT from HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'
and LB-FLAT is a flatted logbook entry list as given by
`org-sql--flatten-lb-entries'."
  (while lb-flat
    ;; Need two of the next entries here because clocks may
    ;; have notes associated with them, but the only
    ;; distinguishing characteristic they have is that they
    ;; don't match anything in org-log-note-headings. If we
    ;; end up processing two entries at once, skip over two
    ;; instead of one on the next iteration.
    (let* ((cur1 (car lb-flat))
           (cur2 (cadr lb-flat))
           (type1 (org-element-type cur1))
           (type2 (org-element-type cur2))
           (try-clock-note (and (eq type1 'clock) (eq type2 'item))))
      (if try-clock-note
          (setq acc (org-sql--extract-lb-clock acc cur1 hl-part cur2)
                lb-flat (cddr lb-flat))
        (setq acc (org-sql--extract-lb-one acc cur1 hl-part)
              lb-flat (cdr lb-flat)))))
  acc)

(defun org-sql--extract-lb-drawer (acc hl-part)
  "Given HL-PART, find logbook drawer and add to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'."
  (--> hl-part
       (alist-get :section it)
       (--first (equal org-log-into-drawer
                       (org-element-property :drawer-name it))
                it)
       (org-element-contents it)
       (org-sql--flatten-lb-entries it)
       (org-sql--extract-lb-contents acc it hl-part)))

(defun org-sql--extract-properties (acc hl-part)
   "Add properties data from HL-PART and add to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'."
   (if (eq 'all org-sql-ignored-properties) acc
     (let ((node-props
            (->> hl-part
                 (alist-get :section)
                 (assoc 'property-drawer)
                 (org-element-contents)
                 (--remove (member (org-element-property :key it)
                                   org-sql--ignored-properties-default))
                 (--remove (member (org-element-property :key it)
                                   org-sql-ignored-properties))))
           (from
            (lambda (acc np hl-part)
              (->>
               (list :file_path (alist-get :filepath hl-part)
                     :headline_offset (->>
                                       hl-part
                                       (alist-get :headline)
                                       (org-element-property :begin))
                     :property_offset (org-element-property :begin np)
                     :key_text (org-element-property :key np)
                     :val_text (->>
                                np
                                (org-element-property :value)
                                org-sql--parse-ts-maybe)
                     ;; TODO add inherited flag
                     :inherited nil)
               (org-sql--alist-put acc 'properties)))))
       (org-sql--extract acc from node-props hl-part))))

(defun org-sql--extract-tags (acc hl-part)
  "Extract tags data from HL-PART and add to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'."
  (if (eq 'all org-sql-ignored-tags) acc
    (let* ((hl (alist-get :headline hl-part))
           ;; first retrieve tags and strip text props and whitespace
           (tags (--> hl
                      (org-element-property :tags it)
                      (mapcar #'org-sql--strip-string it)
                      (-difference it org-sql-ignored-tags)))
           ;; split-string returns nil if it gets ""
           (i-tags (->
                    (org-element-property :ARCHIVE_ITAGS hl)
                    (or "")
                    split-string
                    (-difference org-sql-ignored-tags)))
           ;; then retrieve i-tags, optionally going up to parents
           (i-tags (when org-sql-use-tag-inheritance
                     (org-sql--element-parent-tags hl i-tags)))
           (from
            (lambda (acc tag hl-part &optional inherited)
              (->>
               (list :file_path (alist-get :filepath hl-part)
                     :headline_offset (->>
                                       hl-part
                                       (alist-get :headline)
                                       (org-element-property :begin))
                     :tag tag
                     :inherited (if inherited 1 0))
                (org-sql--alist-put acc 'tags)))))
      (-> acc
          (org-sql--extract from tags hl-part)
          (org-sql--extract from i-tags hl-part t)))))

(defun org-sql--extract-links (acc hl-part)
  "Add link data from headline HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'."
  (if (eq 'all org-sql-ignored-link-types) acc
    (let* ((links (--> (alist-get :section hl-part)
                       (org-element-map it 'link #'identity)
                       (--remove
                        (member (org-element-property :type it)
                                org-sql-ignored-link-types)
                        it)))
           (from
            (lambda (acc ln hl-part)
              (let ((hl (alist-get :headline hl-part)))
                (->>
                 (list :file_path (alist-get :filepath hl-part)
                       :headline_offset (org-element-property :begin hl)
                       :link_offset (org-element-property :begin ln)
                       :link_path (org-element-property :path ln)
                       :link_text (->> ln
                                       org-element-contents
                                       org-element-interpret-data
                                       org-sql--strip-string)
                       :link_type (org-element-property :type ln))
                 (org-sql--alist-put acc 'links))))))
      (org-sql--extract acc from links hl-part))))

(defun org-sql--extract-ts (acc ts hl-part &optional pt)
  "Add timestamp TS data from headline HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'.
PT is a string representing the planning type and is one of 'closed,'
'scheduled,' or 'deadline' although these values are not enforced by
this function."
  (let* ((ts-range (org-sql--parse-ts-range ts))
         (get-resolution
          (lambda (ts)
            (when ts
              (if (org-element-property :hour-start ts)
                  'minute
                'day))))
         (ts-range-res (org-sql--parse-ts-range ts get-resolution))
         (fp (alist-get :filepath hl-part))
         (ts-offset (org-element-property :begin ts))
         (ts-data
          (list
           :file_path fp
           :headline_offset (->> hl-part
                                 (alist-get :headline)
                                 (org-element-property :begin))
           :timestamp_offset ts-offset
           ;; don't care if they are ranges here, this is reflected in
           ;; the time_end value
           :type (--> ts
                      (org-element-property :type it)
                      (cond ((eq it 'inactive-range) 'inactive)
                            ((eq it 'active-range) 'active)
                            (t it)))
           :planning_type pt
           :warning_type (org-element-property :warning-type ts)
           :warning_value (org-element-property :warning-value ts)
           :warning_unit (org-element-property :warning-unit ts)
           :repeat_type (org-element-property :repeater-type ts)
           :repeat_value (org-element-property :repeater-value ts)
           :repeat_unit (org-element-property :repeater-unit ts)
           :time (car ts-range)
           :resolution (car ts-range-res)
           :time_end (cdr ts-range)
           :resolution_end (cdr ts-range-res)
           :raw_value (org-element-property :raw-value ts))))
    (org-sql--alist-put acc 'timestamp ts-data)))

(defun org-sql--extract-hl-contents (acc hl-part)
  "Add contents from partitioned header HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'."
  ;; start by getting the 'section contents, which is everything
  ;; except for the planning element, logbook drawer, and property
  ;; drawer
  (let* ((sec (->>
               hl-part
               (alist-get :section)
               (--remove (eq 'planning (org-element-type it)))
               (--remove (eq 'property-drawer (org-element-type it)))
               (--remove (equal (org-element-property :drawer-name it)
                                org-log-into-drawer))))
         (sec-split (--split-with
                     (or (eq 'clock (org-element-type it))
                         (eq 'plain-list (org-element-type it)))
                     sec))
         (lb-split (-> sec-split car org-sql--split-lb-entries))
         (sec-rem (->> sec-split cdr (append (cdr lb-split))))
         (hl-ts
          (->>
           (org-element-map sec-rem 'timestamp #'identity)
           (--filter (memq (org-element-property :type it)
                           org-sql-included-contents-timestamp-types)))))
    (->
     acc
     (org-sql--extract-lb-contents (car lb-split) hl-part)
     (org-sql--extract #'org-sql--extract-ts hl-ts hl-part))))

(defun org-sql--extract-hl-planning (acc hl-part)
  "Add planning timestamps from HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'.
This will include planning timestamps according to
`org-sql-included-headline-planning-types'."
  (let ((hl (alist-get :headline hl-part)))
    (dolist (type org-sql-included-headline-planning-types)
      (let* ((ts (org-element-property type hl))
             (pt (org-sql--kw-to-colname type)))
        (when ts
          (setq acc (org-sql--extract-ts acc ts hl-part pt)))))
    acc))

(defun org-sql--extract-hl-meta (acc hl-part)
  "Add general data from headline HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql--partition-headline'."
  (let* ((hl (alist-get :headline hl-part))
         (hl-data
          (list
           :file_path (alist-get :filepath hl-part)
           :headline_offset (org-element-property :begin hl)
           :tree_path (org-sql--element-parent-tree-path hl)
           :headline_text (org-element-property :raw-value hl)
           :keyword (->> hl
                         (org-element-property :todo-keyword)
                         org-sql--strip-string)
           :effort (--> hl
                        (org-element-property :EFFORT it)
                        (org-sql--effort-to-int it t))
           :priority (-some->> hl
                               (org-element-property :priority)
                               byte-to-string)
           :archived (if (org-element-property :archivedp hl) 1 0)
           :commented (if (org-element-property :commentedp hl) 1 0)
           :content nil)))
    (->
     acc
     (org-sql--alist-put 'headlines hl-data)
     (org-sql--extract-hl-contents hl-part)
     (org-sql--extract-hl-planning hl-part))))

(defun org-sql--extract-hl (acc headlines fp)
  "Extract data from HEADLINES and add to accumulator ACC.
FP is the path to the file containing the headlines."
  (let ((from
         (lambda (acc hl fp)
           (let* ((hl-part (org-sql--partition-headline hl fp))
                  (hl-sub (alist-get :subheadlines hl-part)))
             (-> acc
                 (org-sql--extract-hl-meta hl-part)
                 (org-sql--extract-links hl-part)
                 (org-sql--extract-tags hl-part)
                 (org-sql--extract-properties hl-part)
                 (org-sql--extract-lb-drawer hl-part)
                 (org-sql--extract-hl hl-sub fp))))))
    (org-sql--extract acc from headlines fp)))

(defun org-sql--extract-file (cell acc)
  "Extract the file in the car of CELL for a sql insertion.
The results are accumulated in ACC which is returned on exit."
  (let* ((fp (car cell))
         (md5sum (cdr cell))
         (fsize (->> fp file-attributes file-attribute-size))
         (headlines (-->
                     fp
                     (find-file-noselect it t)
                     (with-current-buffer it (org-element-parse-buffer))
                     (org-element-contents it)
                     (if (assoc 'section it) (cdr it) it)))
         (file-data (list :file_path fp
                          :md5 md5sum
                          :size fsize)))
    (-> acc
        (org-sql--alist-put 'files file-data)
        (org-sql--extract-hl headlines fp))))

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

(defun org-sql-get-transactions ()
  "Return plist of the transactions to be performed on the db.
The plist has three properties (delete, insert, update) for the three
type of commands that are performed on the database during an update."
  (let ((fp-dsk (org-sql-files-on-disk))
        (map-trns
         (lambda (op fun trans)
           (->>
            (plist-get trans op)
            (--map (funcall fun it))
            org-sql--fmt-trans
            (plist-put trans op)))))
    (->>
     (org-sql-files-in-db)
     (org-sql-sync-all fp-dsk)
     (funcall map-trns 'insert #'org-sql--fmt-inserts)
     (funcall map-trns 'update #'org-sql--fmt-updates)
     (funcall map-trns 'delete #'org-sql--fmt-deletes))))

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
    ;; to defer foreign key constraints, but it is easy to debug now
    ;; so whatever
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
  ;; TODO add debug output here
  (org-sql-update-db)
  (message "Org SQL update complete"))

(defun org-sql-user-clear-all ()
  "Remove all entries in the database."
  (interactive)
  (if (y-or-n-p "Really clear all? ")
      (progn
        (org-sql-cmd-open-connection)
        (message "Clearing Org SQL database")
        (org-sql-clear-db)
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
        (org-sql-init-db)
        (message "Org SQL reset completed"))
    (message "Aborted")))

(provide 'org-sql)
;;; org-sql.el ends here
