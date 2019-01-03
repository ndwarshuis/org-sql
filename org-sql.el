;;; org-sql.el --- SQL backend for Org-Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode
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

;; TODO add description

;;; Code:

;;;; require other code

(require 'dash)
(require 'sql)
(require 'org)

;;;; constants and customizations

(defconst org-sql-ignored-properties-default
  '("ARCHIVE_ITAGS" "Effort")
  "Property keys to be ignored when inserting in properties table. 
It is assumed these are used elsewhere and thus it would be redundant 
to store them. This is in addition to any properties specifified by
`nd/org-sql-ignored-properties'.")

;; TODO, make a formating function to convert a lisp obj to schema
(defconst org-sql-schemas
  '("CREATE TABLE files (file_path TEXT PRIMARY KEY ASC,md5 TEXT NOT NULL,size INTEGER NOT NULL,time_modified DATE,time_created DATE,time_accessed DATE);"
    "CREATE TABLE headlines (file_path TEXT,headline_offset INTEGER,tree_path TEXT,headline_text TEXT NOT NULL,time_closed DATE,time_scheduled DATE,time_deadlined DATE,keyword TEXT,effort INTEGER,priority INTEGER,content TEXT,PRIMARY KEY (file_path ASC, headline_offset ASC),FOREIGN KEY (file_path) REFERENCES files (file_path) ON UPDATE CASCADE ON DELETE CASCADE);"
    "CREATE TABLE tags (file_path TEXT,headline_offset INTEGER,tag TEXT,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path, headline_offset, tag, inherited));"
    "CREATE TABLE properties (file_path TEXT,headline_offset INTEGER,property_offset INTEGER,key_text TEXT NOT NULL,val_text TEXT NOT NULL,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, property_offset ASC));"
    "CREATE TABLE clocking (file_path TEXT,headline_offset INTEGER,clock_offset INTEGER,time_start DATE,time_end DATE,clock_note TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset)ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, clock_offset ASC));"
    "CREATE TABLE logbook (file_path TEXT,headline_offset INTEGER,entry_offset INTEGER,time_logged DATE,header TEXT,note TEXT,FOREIGN KEY (file_path, headline_offset)REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
    "CREATE TABLE state_changes (file_path TEXT,entry_offset INTEGER,state_old TEXT NOT NULL,state_new TEXT NOT NULL,FOREIGN KEY (file_path, entry_offset) REFERENCES logbook (file_path, entry_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
    "CREATE TABLE planning_changes (file_path TEXT,entry_offset INTEGER,time_old DATE NOT NULL,time_new DATE,planning_type TEXT CHECK (planning_type = \"d\" or (planning_type = \"s\")),FOREIGN KEY (file_path, entry_offset) REFERENCES logbook (file_path, entry_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
    "CREATE TABLE links (file_path TEXT,headline_offset INTEGER,link_offset INTEGER,link_path TEXT,link_text TEXT,link_type TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, link_offset ASC));")
  "Table schemas for the org database.")

;; TODO add better docs here...this is too thin
(defgroup org-sql nil
  "Org mode SQL backend options."
  :tag "Org SQL"
  :group 'org)
  
(defcustom org-sql-use-tag-inheritance nil
  "Use tag inheritance when constructing sql databases for org.
Mirrors behavior of `org-use-tag-inheritance'."
  :type 'boolean
  :group 'org-sql)

(defcustom org-sqlite-db-path (expand-file-name "org.db" org-directory)
  "Path for the sqlite database that holds archive data."
  :type 'file
  :group 'org-sql)

(defcustom org-sql-ignored-properties nil
  "Property keys to be ignored when inserting in properties table."
  :type '(repeat :tag "List of properties to ignore." string)
  :group 'org-sql)

(defcustom org-sql-files nil
  "A list of org files or directories to put into sql database."
  :type '(repeat :tag "List of files and directories" file)
  :group 'org-sql)
  
(defcustom org-sql-default-pragma
  '(:foreign_keys on :defer_foreign_keys on)
  "Default pragmas used when opening a sql connection
via `org-sql-cmd-open-connection'."
  :type '(plist :key-type symbol :value-type string)
  :group 'org-sql)

(defcustom org-sql-buffer "*SQL: Org*"
  "Name of the SQLi buffer connected to the database."
  :type 'string
  :group 'org-sql)

;; TODO this makes too much sense
;; (defconst org-sql-debug-buffer "*SQL: Org-Debug*"
;;   "Name of the SQLi buffer connected to the database.")

;;;; helper functions

(defun org-sql-strip-string (str)
  "Remove text properties and trim STR and return the result."
  (when str (string-trim (substring-no-properties str))))
  
(defun org-sql-alist-put (alist prop value &optional front)
  "For given ALIST, append VALUE to the current values in prop.
Current values (that is the cdr of each key) is assumed to be a list.
If PROP does not exist, create it. Return the new alist. If FRONT is 
t, add to the front of current values list instead of the back."
  (let* ((cur-cell (assoc prop alist))
         (cur-values (cdr cur-cell)))
      (cond
       (cur-values
        (let ((new-cdr (if front
                           `(,value ,@cur-values)
                         `(,@cur-values ,value))))
          (setcdr cur-cell new-cdr) alist))
       (cur-cell
        (setcdr cur-cell `(,value)) alist)
       (alist
        (append alist `((,prop ,value))))
       (t 
        `((,prop ,value))))))
        
(defmacro org-sql-with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body)) (indent 1))
  `(progn
     ,@(--map (cons 'advice-add it) adlist)
     (unwind-protect (progn ,@body)
       ,@(--map `(advice-remove ,(car it) ,(nth 2 it)) adlist))))

(defun org-sql-plist-get-vals(plist)
  "Return all the values in PLIST."
  (-slice plist 1 nil 2))

;;;; SQL string parsing functions

(defun org-sql-to-plist (out cols)
  "Parse SQL output string OUT to an plist representing the data.
COLS are the column names as symbols used to obtain OUT."
  (unless (equal out "")
    (-->
     out
     (string-trim it)
     (split-string it "\n")
     (mapcar (lambda (s) (split-string s "|")) it)
     (mapcar (lambda (s) (-interleave cols s)) it))))

;;;; SQL formatting helper functions

(defun org-sql-escape-text (txt)
  "Escape and quote TXT in order to insert into sqlite db via 'insert'.
This assumes the insertion command will be run on a shell where the
sql command string is in double quotes."
  (->> txt
       (replace-regexp-in-string "'" "''")
       (replace-regexp-in-string "\n" "'||char(10)||'")
       (format "'%s'")))

(defun org-sql-to-string (entry)
  "Convert ENTRY to a string suitable for insertion into SQLite db.
Converts numbers to strings, flanks strings with '\"', and converts 
any other symbols to their symbol name."
  (cond ((stringp entry) (org-sql-escape-text entry))
        ((numberp entry) (number-to-string entry))
        (entry (symbol-name entry))
        (t "NULL")))

(defun org-sql-kw-to-colname (kw)
  "Return string representation of KW for column in sql database."
  (--> kw (symbol-name it) (substring it 1)))

(defun org-sql-plist-concat (plist &optional sep)
  "Concatenate a PLIST to string to be used in a SQL statement.
Returns a string formatted like 'prop1 = value1 SEP prop2 = value2'
from a plist like '(:prop1 value1 :prop2 value2)."
  (let ((sep (or sep ","))
        (keys (->> plist
                   plist-get-keys
                   (mapcar #'org-sql-kw-to-colname)))
        (vals (->> plist
                   org-sql-plist-get-vals
                   (mapcar #'org-sql-to-string))))
    (-some-->
     (--zip-with (format "%s=%s" it other) keys vals)
     (string-join it sep))))

;;;; SQL command formatting functions

(defun org-sql-fmt-insert (tbl-name tbl-data)
  "Format SQL insert command from TBL-NAME and TBL-DATA."
  (let ((col-names (-->
                    tbl-data
                    (plist-get-keys it)
                    (mapcar #'org-sql-kw-to-colname it)
                    (string-join it ",")))
        (col-values (-->
                     tbl-data
                     (org-sql-plist-get-vals it)
                     (mapcar #'org-sql-to-string it)
                     (string-join it ","))))
    (format "insert into %s (%s) values (%s);" (symbol-name tbl-name)
            col-names col-values)))

(defun org-sql-fmt-update (tbl-name update)
  "Format SQL update command from TBL-NAME, UPDATE, and CONDS."
  (let ((upd-str (->> update car org-sql-plist-concat))
        (conds-str (--> update (cdr it) (org-sql-plist-concat it " and "))))
    (format "update %s set %s where %s;" (symbol-name tbl-name)
            upd-str conds-str)))

(defun org-sql-fmt-delete (tbl-name conds)
  "Format SQL update command from TBL-NAME and CONDS."
  (--> conds
       (org-sql-plist-concat it " and ")
       (format "delete from %s where %s;" (symbol-name tbl-name) it)))

(defun org-sql-fmt-trans (sql-str)
  "Format SQL transaction from list of SQL commands as strings SQL-STR."
  (-some->> sql-str
            (-flatten)
            (string-join)
            (format "begin transaction; %s commit;")))

(defun org-sql-fmt-multi (tbl fun)
  (--map (funcall fun (car tbl) it) (cdr tbl)))

(defun org-sql-fmt-inserts (tbl)
  (org-sql-fmt-multi tbl #'org-sql-fmt-insert))

(defun org-sql-fmt-updates (tbl)
  (org-sql-fmt-multi tbl #'org-sql-fmt-update))

(defun org-sql-fmt-deletes (tbl)
  (org-sql-fmt-multi tbl #'org-sql-fmt-delete))

(defun org-sql-fmt-pragma (plist)
  "Creates a SQL statement for setting pragmas in PLIST.
PLIST contains the pragmas as the properties and their intended
values as the property values."
  (let ((pragmas (->> plist
                      plist-get-keys
                      (mapcar #'org-sql-kw-to-colname))))
    (->> plist
         org-sql-plist-get-vals
         (--zip-with (format "PRAGMA %s=%s;" it other) pragmas)
         string-join)))
  
;;;; SQL command abstractions

(defun org-sql-cmd-open-connection ()
  "Open a new SQL connection to `org-sqlite-db-path'.
This also sets the pragma according to `org-sql-default-pragma'."
  (org-sql-with-advice
      ((#'sql-get-login :override #'ignore)
       (#'pop-to-buffer :override #'ignore))
    (let ((sql-database org-sqlite-db-path))
      (sql-sqlite org-sql-buffer)
      (org-sql-cmd-set-pragma))))

;; TODO this can be put in terms of a better data struct
(defun org-sql-pragma-merge-default (&optional pragma)
  "Override values in `org-sql-default-pragma' with PRAGMA.
PRAGMA is a plist as described in `org-sql-fmt-pragma'. Return a
new plist with values from PRAGMA either added (if they don't already 
exist) to or instead of (if they already exist) those in 
`org-sql-default-pragma'."
  (if (not pragma)
      org-sql-default-pragma
    (let ((all-props
           (->>
            org-sql-default-pragma
            plist-get-keys
            (append (plist-get-keys pragma))
            delete-dups))
          (getv
           (lambda (p)
             (or (plist-get pragma p)
                 (plist-get org-sql-default-pragma p)))))
      (mapcan (lambda (p) `(,p ,(funcall getv p))) all-props))))

(defun org-sql-cmd-set-pragma (&optional pragma)
  (->> pragma
       org-sql-pragma-merge-default
       org-sql-fmt-pragma
       org-sql-cmd))
  
(defun org-sql-cmd (cmd)
  "Execute SQL string CMD in SQLi buffer given by `org-sql-buffer'.
If buffer process not running, it is started automatically. Returns
the output of CMD as given by the running SQL shell."
  (when cmd
    (org-sql-with-advice
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

(defun org-sql-cmd-select (db tbl-name &optional cols conds)
  "Select columns from TBL-NAME in DB where COLS is the list of columns.
If COLS is nil, all columns will be returned. Columns is expected as
a list of keywords like ':col1' and :col2'. CONDS, if supplied, is
a plist of conditions to test in the select statement. (currently
joined by AND)"
  (let* ((colnames
          (if (not cols) "*"
            (--> cols
                 (mapcar #'org-sql-kw-to-colname it)
                 (string-join it ","))))
         (tbl-str (symbol-name tbl-name))
         (cmd (if (not conds)
                  (format "select %s from %s;" colnames tbl-str)
                (--> conds
                     (org-sql-plist-concat it " and ")
                     (format "select %s from %s where %s;" colnames
                             tbl-str it)))))
    (--> cmd (org-sql-cmd it) (org-sql-to-plist it cols))))

;;;; org-mode string parsing functions

(defun org-sql-effort-to-int (effort-str &optional to-string throw-err)
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

(defun org-sql-ts-fmt-iso (ts)
  "Return org timestamp TS to as string in ISO 8601 format.
If TS is nil or TS cannot be understood, nil will be returned."
  (-some-->
   ts
   (save-match-data (org-2ft it))
   (when (> it 0) (format-time-string "%Y-%m-%dT%H:%M:00" it))))

(defun org-sql-parse-ts-range (ts)
  "Return start and end of timestamp TS depending on if it is a range.
Return value will be a list of two elements if range and one if not."
  (when ts
    (let ((split
           (lambda (ts &optional end)
             (--> ts
                  (org-timestamp-split-range it end)
                  (org-element-property :raw-value it)
                  (org-sql-ts-fmt-iso it)))))
      (if (eq (org-element-property :type ts) 'inactive-range)
          (let ((start (funcall split ts))
                (end (funcall split ts t)))
            (cons start end))
        `(,(funcall split ts))))))

(defun org-sql-parse-ts-maybe (txt)
  "If TXT is a timestamp, return it in ISO 8601 format.
Otherwise return it unchanged."
  ;; assume the iso parser to return nil on failure
  (-> txt org-sql-ts-fmt-iso (or txt)))

;;;; org-mode element helper functions

(defun org-sql-element-ts-raw (prop obj &optional iso)
  "Return the raw-value of the timestamp PROP in OBJ if exists.
If ISO is t, return the timestamp in ISO 8601 format."
  (-some--> obj
            (org-element-property prop it)
            (org-element-property :raw-value it)
            (if iso (org-sql-ts-fmt-iso it) it)))
            
(defun org-sql-element-split-by-type (type contents &optional right)
  "Split org-element sequence of objects CONTENTS by first instance of TYPE.
If RIGHT is t, get the right half instead of the left."
  (letrec ((scan
            (lambda (c &optional acc)
              (if c
                  (let ((cur (car c))
                        (rem (cdr c)))
                    (if (equal type (org-element-type cur))
                        (if right rem acc)
                      (funcall scan rem (append acc (list cur)))))
                (unless right acc)))))
    (funcall scan contents)))
        
(defun org-sql-element-parent-headline (obj)
  "Get the parent headline element (if any) of org-element OBJ."
  (when obj
    (let ((parent (org-element-property :parent obj)))
      (if (eq 'headline (org-element-type parent))
          parent
        (org-sql-element-parent-headline parent)))))
        
(defun org-sql-element-parent-tree-path (obj &optional acc)
  "Construct parent tree path for object OBJ and concatenate to ACC.
Returns '/' delimited path of headlines or nil if obj is in a toplevel
headline."
  (let ((parent-hl (org-sql-element-parent-headline obj)))
    (if parent-hl
        (--> parent-hl
             (org-element-property :raw-value it)
             (concat "/" it acc)
             (org-sql-element-parent-tree-path parent-hl it))
      acc)))
      
(defun org-sql-element-parent-tags (obj &optional acc)
  "Get all tags from parent headlines of OBJ and concat to ACC.
ACC is treated as a set; therefore no duplicates are retained."
  (let ((parent-hl (org-sql-element-parent-headline obj)))
    (if parent-hl
        (let* ((tags (->>
                      parent-hl
                      (org-element-property :tags)
                      (mapcar #'org-sql-strip-string)))
               (i-tags (org-element-property :ARCHIVE_ITAGS parent-hl))
               (i-tags (when i-tags (split-string i-tags)))
               (all-tags (delete-dups (append acc tags i-tags))))
          (org-sql-element-parent-tags parent-hl all-tags))
      acc)))

(defun org-sql-lb-match-header (header-text)
  "Attempts to match HEADER-TEXT with `org-sql-log-note-headings-regexp'.
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
           (type (funcall scan header-text org-sql-log-note-headings-regexp)))
    (when type (cons type (match-data)))))
    
(defun org-sql-todo-keywords ()
 "Return `org-todo-keywords' as string list w/o selectors.
Will likely match the value of `org-todo-keywords-1' in many cases,
but this has the advantage of being always available and comprehensive."
 (->>
  org-todo-keywords
  copy-tree
  (mapcan #'cdr)
  (remove "|")
  (--map (replace-regexp-in-string "(.*)" "" it))))

(defun org-sql-log-note-headings-convert ()
  "Convert `org-log-note-headings' to a regex matcher.
See `org-log-note-headings' for escape sequences that are matched
and replaces by regexps that match what would be inserted in place
of the escapes."
  (let* ((escapes '("%u" "%U" "%t" "%T" "%d" "%D" "%s" "%S"))
         (ts-or-todo-regexp
          (-->
           (org-sql-todo-keywords)
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

(defconst org-sql-log-note-headings-regexp
  (org-sql-log-note-headings-convert)
  "Like `org-log-note-headings' but has regexp's instead of
escape sequences.")

;;;; org-mode element partitioning functions

(defun org-sql-partition-headline (headline fp)
  "For org-element HEADLINE and file path FP, return an alist.
The alist will be structured as such:

:filepath - path to the file in which the headline resides
:headline - original headline element
:section - the section contents of the headline if found
:subheadlines - list of subheadlines if any

The planning entry will have the list of data associated with the
:planning property, and likewise with property-drawer. logbook-drawer
will be a drawer that is explicitly named `org-log-into-drawer' or
nil if not set. other-contents includes all other elements including
other drawers, list, paragraph elements, etc. If any of these groups 
are missing, nil will be returned."
 (unless headline (error "No headline given"))
 (unless fp (error "No file path given"))
 (let* ((hl-contents (org-element-contents headline))
        (section (->> hl-contents (assoc 'section) org-element-contents))
        (subheadlines (if section (cdr hl-contents) hl-contents)))
   `((:headline . ,headline)
     (:filepath . ,fp)
     (:section . ,section)
     (:subheadlines . ,subheadlines))))

(defun org-sql-partition-item (item hl-part)
  "Parse an org-element ITEM which is assumed to be part of a logbook.
Returns a alist with the following structure:

:hl-part - the partitioned headline HL-PART surrounding the item,
  which is an object as described in `org-sql-partition-headline'
:item - the original item element
:header-text - the first line of the note which is standardized using
  `org-log-note-headings'
:note-text - the remainder of the note text as a trimmed string with
  no text properties (will be nil if item has no line-break element)
:type - the type of the item's header text (may be nil if unknown)
:match-data - match data associated with finding the type as done
  using `org-sql-log-note-headings-regexp' (may be nil if undetermined).

Anatomy of a logbook item (non-clocking):
- header-text with linebreak //
  note-text ... more text
- another header-text linebreak

The header text is solely used for determining :type and :match-data."
  (let* ((contents (->> item (assoc 'paragraph) org-element-contents))
         (header-text (->> contents
                           (org-sql-element-split-by-type 'line-break)
                           org-element-interpret-data
                           org-sql-strip-string))
         (note-text (--> contents
                         (org-sql-element-split-by-type 'line-break it t)
                         org-element-interpret-data
                         org-sql-strip-string))
         (header-match (org-sql-lb-match-header header-text)))
    `((:item . ,item)
      (:hl-part . ,hl-part)
      (:header-text . ,header-text)
      (:note-text . ,note-text)
      (:type . ,(car header-match))
      (:match-data . ,(cdr header-match)))))

;;;; org element extraction functions

(defun org-sql-extract (acc fun objs &rest args)
  "Iterate through OBJS and add them to accumulator ACC using FUN.
FUN is a function that takes a single object from OBJS, the accumulator,
and ARGS. FUN adds OBJ to ACC and returns a new ACC."
  (while objs
    (setq acc (apply fun acc (car objs) args)
          objs (cdr objs)))
  acc)

(defun org-sql-extract-lb-header (acc item-part)
  "Add specific data from logbook entry ITEM-PART to accumulator ACC.
ITEM-PART is a partitions logbook item as described in
`org-sql-partition-item'. Note headings are parsed according to
how they match those generated by `org-log-note-headings', and
nothing is added if a match is not found."
  (let* ((hl-part (alist-get :hl-part item-part))
         (hl (alist-get :headline hl-part))
         (fp (alist-get :filepath hl-part))
         (item (alist-get :item item-part))
         (item-offset (org-element-property :begin item))
         (type (alist-get :type item-part))
         (md (alist-get :match-data item-part))
         (header-text (alist-get :header-text item-part)))
    ;; TODO, make these adapt to the value of org-log-note-headings??
    (set-match-data md)
    (cond
     ((eq type 'state)
      (let* ((state-old (match-string 3 header-text))
             (state-new (match-string 1 header-text))
             (state-data (list :file_path fp
                               :entry_offset item-offset
                               :state_old state-old
                               :state_new state-new)))
        (org-sql-alist-put acc 'state_changes state-data)))
     ((memq type '(reschedule delschedule redeadline deldeadline))
      (let* ((time-old (->> header-text
                            (match-string 1)
                            org-sql-ts-fmt-iso))
             (planning-kw (if (memq type '(reschedule delschedule))
                              :scheduled
                            :deadline))
             (time-new (org-sql-element-ts-raw planning-kw hl t))
             (planning-type (if (eq :scheduled planning-kw) "s" "d"))
             (planning-data (list :file_path fp
                                  :entry_offset item-offset
                                  :time_old time-old
                                  :time_new time-new
                                  :planning_type planning-type)))
        (org-sql-alist-put acc 'planning_changes planning-data)))
     ;; no action required for these
     ((memq type '(done refile note)) acc)
     ;; header type not determined, therefore do nothing
     (t acc))))

(defun org-sql-item-time-logged (item-part)
  "Return time-logged of ITEM-PART or nil if it cannot be determined.
ITEM-PART is a partitioned logbook item as described in
`org-sql-partition-item'."
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
           org-sql-ts-fmt-iso))))

(defun org-sql-extract-lb-item (acc item-part)
  "Add data from logbook entry ITEM-PART to accumulator ACC.
ITEM-PART is a partitioned logbook item as described in
`org-sql-partition-item'."
  (let* ((hl-part (alist-get :hl-part item-part))
         (fp (alist-get :filepath hl-part))
         (hl (alist-get :headline hl-part))
         (item (alist-get :item item-part))
         (hl-offset (org-element-property :begin hl))
         (item-offset (org-element-property :begin item))
         (time-logged (org-sql-item-time-logged item-part))
         (hdr-text (alist-get :header-text item-part))
         (note-text (alist-get :note-text item-part))
         (logbook-data (list :file_path fp
                             :headline_offset hl-offset
                             :entry_offset item-offset
                             :time_logged time-logged
                             :header hdr-text
                             :note note-text)))
    (-> acc
        (org-sql-alist-put 'logbook logbook-data)
        (org-sql-extract-lb-header item-part))))

(defun org-sql-extract-lb-clock (acc clock hl-part &optional item)
  "Add data from logbook CLOCK to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'
and represents the headline surrounding the clock.
If ITEM is provided, check that this is a valid note that can be
added to the clock, else add it as a normal logbook entry."
  (let* ((hl (alist-get :headline hl-part))
         (fp (alist-get :filepath hl-part))
         (hl-offset (org-element-property :begin hl))
         (cl-offset (org-element-property :begin clock))
         (ts-range (->> clock
                        (org-element-property :value)
                        org-sql-parse-ts-range))
         (start (car ts-range))
         (end (cdr ts-range))
         (clock-data (list :file_path fp
                           :headline_offset hl-offset
                           :clock_offset cl-offset
                           :time_start start
                           :time_end end)))
    (if (not item)
        (org-sql-alist-put acc 'clocking clock-data)
      (let* ((item-part (org-sql-partition-item item hl-part))
             (item-type (alist-get :type item-part)))
        (if item-type
            ;; if we know the type, add the clock and note separately
            (-> acc
                (org-sql-alist-put 'clocking clock-data)
                (org-sql-extract-lb-item item-part))
          ;; else add it with the clocking table
          (->> item-part
               (alist-get :header-text)
               (list :clock_note)
               (append clock-data)
               (org-sql-alist-put acc 'clocking)))))))

(defun org-sql-extract-lb-items (acc items hl-part)
  "Add data from logbook ITEMS to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'
and represents the headline surrounding the items."
  (let ((from
         (lambda (acc item hl-part)
           (->> hl-part
                (org-sql-partition-item item)
                (org-sql-extract-lb-item acc)))))
    (org-sql-extract acc from items hl-part)))

(defun org-sql-extract-lb-one (acc entry hl-part)
  "Add data from logbook ENTRY to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'
and represents the headline surrounding the entry."
  (let ((type (org-element-type entry)))
    (cond
     ((eq type 'clock)
      (org-sql-extract-lb-clock acc entry hl-part))
     ((eq type 'plain-list)
      (--> entry
           (org-element-contents it)
           (org-sql-extract-lb-items acc it hl-part)))
     ;; TODO add an "UNKNOWN" logbook parser
     (t acc))))

(defun org-sql-extract-lb-two (acc entry1 entry2 hl-part)
  "Add data from logbook ENTRY1 and ENTRY2 to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'
and represents the headline surrounding the entries. This assumes the
entries are org-element types clock and plain-list respectively, and
will check if the first item in ENTRY2 is part of the clock."
  (let* ((items (org-element-contents entry2))
         (first-item (car items))
         (rem-items (cdr items)))
    (-> acc
        (org-sql-extract-lb-clock entry1 hl-part first-item)
        (org-sql-extract-lb-items rem-items hl-part))))

(defun org-sql-extract-lb (acc hl-part)
  "Add logbook data from HL-PART and add to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'."
  (let* ((lb-contents
          (->>
           hl-part
           (alist-get :section)
           (--first (equal org-log-into-drawer
                           (org-element-property :drawer-name it)))
           org-element-contents)))
    (while lb-contents
      ;; Need two of the next entries here because clocks may
      ;; have notes associated with them, but the only
      ;; distinguishing characteristic they have is that they
      ;; don't match anything in org-log-note-headings. If we
      ;; end up processing two entries at once, skip over two
      ;; instead of one on the next iteration.
      (let* ((cur1 (car lb-contents))
             (cur2 (cadr lb-contents))
             (type1 (org-element-type cur1))
             (type2 (org-element-type cur2))
             (try-clock-note (and (eq 'clock type1)
                                  (eq type2 'plain-list))))
        (if try-clock-note
            (setq acc (org-sql-extract-lb-two acc cur1 cur2 hl-part)
                  lb-contents (cddr lb-contents))
          (setq acc (org-sql-extract-lb-one acc cur1 hl-part)
                lb-contents (cdr lb-contents)))))
    acc))

(defun org-sql-extract-properties (acc hl-part)
   "Add properties data from HL-PART and add to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'."
  (let ((node-props (->> hl-part
                         (alist-get :section)
                         (assoc 'property-drawer)
                         org-element-contents))
        (from
         (lambda (acc np hl-part)
           (let ((key (org-element-property :key np)))
             ;; TODO this can be better, make a list somewhere else
             ;; and concat once
             (if (member key (-distinct
                              (append
                               org-sql-ignored-properties-default
                               org-sql-ignored-properties)))
                 acc
               (let* ((hl (alist-get :headline hl-part))
                      (fp (alist-get :filepath hl-part))
                      (hl-offset (org-element-property :begin hl))
                      (np-offset (org-element-property :begin np))
                      (val (->> np
                                (org-element-property :value)
                                org-sql-parse-ts-maybe))
                      (prop-data (list :file_path fp
                                       :headline_offset hl-offset
                                       :property_offset np-offset
                                       :key_text key
                                       :val_text val
                                       ;; TODO add inherited flag
                                       :inherited nil)))
                 (org-sql-alist-put acc 'properties prop-data)))))))
    (org-sql-extract acc from node-props hl-part)))

(defun org-sql-extract-tags (acc hl-part)
  "Extract tags data from HL-PART and add to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'."
  (let* ((hl (alist-get :headline hl-part))
         ;; first retrieve tags and strip text props and whitespace
         (tags (->> hl
                    (org-element-property :tags)
                    (mapcar #'org-sql-strip-string)))
         ;; split-string returns nil if it gets ""
         (i-tags (->
                  (org-element-property :ARCHIVE_ITAGS hl)
                  (or "")
                  split-string))
         ;; then retrieve i-tags, optionally going up to parents
         (i-tags (when org-sql-use-tag-inheritance
                     (org-sql-element-parent-tags hl i-tags)))
         (from
          (lambda (acc tag hl-part &optional inherited)
            (let* ((hl (alist-get :headline hl-part))
                   (fp (alist-get :filepath hl-part))
                   (offset (org-element-property :begin hl))
                   (i (if inherited 1 0))
                   (tags-data (list :file_path fp
                                    :headline_offset offset
                                    :tag tag
                                    :inherited i)))
              (org-sql-alist-put acc 'tags tags-data)))))
    (-> acc
        (org-sql-extract from tags hl-part)
        (org-sql-extract from i-tags hl-part t))))

(defun org-sql-extract-links (acc hl-part)
  "Add link data from headline HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'."
  (let* ((sec (alist-get :section hl-part))
         (links (org-element-map sec 'link #'identity))
         (from
          (lambda (acc ln hl-part)
              (let* ((fp (alist-get :filepath hl-part))
                     (hl (alist-get :headline hl-part))
                     (hl-offset (org-element-property :begin hl))
                     (ln-offset (org-element-property :begin ln))
                     (ln-path (org-element-property :path ln))
                     (ln-text (->> ln
                                   org-element-contents
                                   org-element-interpret-data
                                   org-sql-strip-string))
                     (ln-type (org-element-property :type ln))
                     (ln-data (list :file_path fp
                                    :headline_offset hl-offset
                                    :link_offset ln-offset
                                    :link_path ln-path
                                    :link_text ln-text
                                    :link_type ln-type)))
                (org-sql-alist-put acc 'links ln-data)))))
    (org-sql-extract acc from links hl-part)))

(defun org-sql-extract-hl-meta (acc hl-part)
  "Add general data from headline HL-PART to accumulator ACC.
HL-PART is an object as returned by `org-sql-partition-headline'."
  (let* ((fp (alist-get :filepath hl-part))
         (hl (alist-get :headline hl-part))
         (offset (org-element-property :begin hl))
         (rxv-tp (org-sql-element-parent-tree-path hl))
         (hl-txt (org-element-property :raw-value hl))
         ;; (t-created (->> hl
         ;;                 (org-element-property :CREATED)
         ;;                 org-sql-ts-fmt-iso))
         (t-closed (org-sql-element-ts-raw :closed hl t))
         (t-scheduled (org-sql-element-ts-raw :scheduled hl t))
         (t-deadline (org-sql-element-ts-raw :deadline hl t))
         (kw (->> hl
                  (org-element-property :todo-keyword)
                  org-sql-strip-string))
         (effort (--> hl
                      (org-element-property :EFFORT it)
                      (org-sql-effort-to-int it t)))
         (priority (org-element-property :priority hl))
         ;; TODO, add contents somehow
         ;; (hl-contents (plist-get hl-part :hl-contents))
         ;; (hl-contents-text (org-element-interpret-data hl-contents))
         ;; (hl-contents-text (when hl-contents-text
         ;;                     (string-trim
         ;;                      (substring-no-properties hl-contents-text))))
         (hl-data (list :file_path fp
                        :headline_offset offset
                        :tree_path rxv-tp
                        :headline_text hl-txt
                        ;; :time_created t-created
                        :time_closed t-closed
                        :time_scheduled t-scheduled
                        :time_deadlined t-deadline
                        :keyword kw
                        :effort effort
                        :priority priority
                        :content nil)))
    (org-sql-alist-put acc 'headlines hl-data)))

(defun org-sql-extract-hl (acc headlines fp)
  "Extract data from HEADLINES and add to accumulator ACC.
FP is the path to the file containing the headlines."
  (let ((from
         (lambda (acc hl fp)
           (let* ((hl-part (org-sql-partition-headline hl fp))
                  (hl-sub (alist-get :subheadlines hl-part)))
             (-> acc
                 (org-sql-extract-hl-meta hl-part)
                 (org-sql-extract-links hl-part)
                 (org-sql-extract-tags hl-part)
                 (org-sql-extract-properties hl-part)
                 (org-sql-extract-lb hl-part)
                 (org-sql-extract-hl hl-sub fp))))))
    (org-sql-extract acc from headlines fp)))

(defun org-sql-extract-file (cell acc)
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
        (org-sql-alist-put 'files file-data)
        (org-sql-extract-hl headlines fp))))

;;;; database syncing functions

(defun org-sql-sync-insert (cell acc)
  "Add insertion commands for CELL in accumulator ACC. Return new ACC."
  (->> (plist-get acc 'insert)
       (org-sql-extract-file cell)
       (plist-put acc 'insert)))

(defun org-sql-sync-update (cell acc)
  "Add update commands for CELL in accumulator ACC. Return new ACC."
  (let ((updt-acc (plist-get acc 'update)))
    (->> `((:file_path ,(car cell)) . (:md5 ,(cdr cell)))
         (org-sql-alist-put updt-acc 'files)
         (plist-put acc 'update))))

(defun org-sql-sync-delete (cell acc)
  "Add deletion commands for CELL in accumulator ACC. Return new ACC."
  (let ((dlt-acc (plist-get acc 'delete)))
    (->>  `(:file_path ,(car cell))
          (org-sql-alist-put dlt-acc 'files)
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
  (if (find cell fp-qry :test #'equal)
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
           (found-fp (find-if (lambda (q) (funcall match-fp* q)) fp-qry)))
      (cond
       ;; if fp matches, delete qry in db and insert cell
       (found-fp
        (cons (org-sql-sync-insert cell (org-sql-sync-delete found-fp acc))
              (remove found-fp fp-qry)))
       ;; if md5 matches, update fp in db
       ((find-if (lambda (q) (funcall match-md5* q)) fp-qry)
        (cons (org-sql-sync-update cell acc)
              (remove-if (lambda (q) (funcall match-md5* q)) fp-qry)))
       ;; if none match, insert cell
       (t
        (cons (org-sql-sync-insert cell acc) fp-qry))))))

;; TODO, need to document the accumulator somewhere
(defun org-sql-sync-all (fp-dsk fp-qry)
  "Synchronize state b/t disk and db represented by FP-DSK and FP-QRY.
These are lists of cons cells as returned via `org-sql-files-in-disk'
and `org-sql-files-in-db' respectively. This function iterates through
all cells in FP-QRY, interrogating their sync state via 
`org-sql-sync-one' (this takes care of any insertion and update
operations for cells in FP-DSK). Anything in FP-QRY that is not matched
with anything in FP-DSK is assumed to be deleted and is removed at the
end of this function.

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
  "Return full list of absolute file paths via `org-sql-files' variable."
  (->>
   org-sql-files
   (--map (if (file-directory-p it)
              (directory-files it t "\\`.*\\.org\\(_archive\\)?\\'")
            (list it)))
   (apply #'append)))

(defun org-sql-files-on-disk ()
  "Return alist for file paths in `org-sql-files'.
In each cell, the car is the file path and cdr is the md5sum."
  (let ((cons-md5
         (lambda (fp)
           (--> fp (find-file-noselect it t) (md5 it) (cons fp it)))))
    (->> (org-sql-files) (--map (funcall cons-md5 it)))))

(defun org-sql-files-in-db ()
  "Get all files and their metadata from the database.
Returns an alist where the each car is file_path and each cdr is
the plist of metadata."
  ;; TODO should probably make the table recreate itself if it is
  ;; corrupted or missing
  (when (file-exists-p org-sqlite-db-path)
    (->> '(:file_path :md5)
         (org-sql-cmd-select org-sqlite-db-path 'files)
         (mapcar #'org-sql-plist-get-vals)
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
            org-sql-fmt-trans
            (plist-put trans op)))))
    (->>
     (org-sql-files-in-db)
     (org-sql-sync-all fp-dsk)
     (funcall map-trns 'insert #'org-sql-fmt-inserts)
     (funcall map-trns 'update #'org-sql-fmt-updates)
     (funcall map-trns 'delete #'org-sql-fmt-deletes))))

(defun org-sql-init-db ()
  "Add schemas to database if they do not exist already.
This assumes an active connection is open."
  ;; assume that the db will be created when a new connection is opened
  (->> org-sql-schemas (mapcar #'org-sql-cmd)))

(defun org-sql-update-db ()
  "Update the database. This assumes an active connection is open."
  (let ((trans (org-sql-get-transactions)))
    ;; the order below likely doesn't matter if the pragma is set
    ;; to defer foreign key constraints, but it is easy to debug now
    ;; so whatever
    `(,(-> trans (plist-get 'delete) (org-sql-cmd))
      ,(-> trans (plist-get 'update) (org-sql-cmd))
      ,(-> trans (plist-get 'insert) (org-sql-cmd)))))

;;;; interactive user functions

(defun org-sql-user-update ()
  "Update the Org SQL database."
  (interactive)
  ;; TODO need to see if schema is correct?
  ;; for now this assumes the db exists and has a valid schema
  (unless (get-buffer-process org-sql-buffer)
    (message "Opening SQLi Buffer")
    (org-sql-cmd-open-connection))
  (message "Updating Org SQL database")
  (org-sql-update-db)
  (message "Org SQL update complete"))

(provide 'org-sql)
;;; test.el ends here
