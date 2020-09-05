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

;; TODO this assumes that columns b/t foreign keys and refs are the same name
;; TODO add compile check to make sure the columns in the constraints exist
(eval-and-compile
  (defconst org-sql--metaschema
    '((files
       (desc . "Each row stores metadata for one tracked org file")
       (columns
        (:file_path :desc "path to the org file"
                    :type text)
        (:md5 :desc "md5 checksum of the org file"
              :type text
              :constraints (notnull))
        (:size :desc "size of the org file in bytes"
               :type integer
               :constraints (notnull)))
       ;; (:time_modified :type integer)
       ;; (:time_created :type integer)
       ;; (:time_accessed :type integer))
       (constraints
        (primary :keys (:file_path asc))))

      (headlines
       (desc . "Each row stores one headline in a given org file and its metadata")
       (columns
        (:file_path :desc "path to file containin the headline"
                    :type text)
        (:headline_offset :desc "file offset of the headline's first character"
                          :type integer)
        (:tree_path :desc "outline tree path of the headline"
                    :type text)
        (:headline_text :desc "raw text of the headline"
                        :type text
                        :constraints (notnull))
        (:keyword :desc "the TODO state keyword"
                  :type text)
        (:effort :desc "the value of the Effort property in minutes"
                 :type integer)
        (:scheduled_offset :desc "file offset of the SCHEDULED timestamp"
                           :type integer)
        (:deadline_offset :desc "file offset of the DEADLINE timestamp"
                          :type integer)
        (:closed_offset :desc "file offset of the CLOSED timestamp"
                        :type integer)
        (:priority :desc "character value of the priority"
                   :type char)
        (:is_archived :desc "true if the headline has an archive tag"
                   :type boolean
                   :constraints (notnull))
        (:is_commented :desc "true if the headline has a comment keyword"
                    :type boolean
                   :constraints (notnull))
        (:content :desc "the headline contents (currently unused)"
                  :type text))
       (constraints
        (primary :keys (:file_path asc :headline_offset asc))
        (foreign :ref files
                 :keys (:file_path)
                 :parent-keys (:file_path)
                 :on_delete cascade
                 :on_update cascade)))

      (tags
       (desc . "Each row stores one tag")
       (columns
        (:file_path :desc "path to the file containing the tag"
                    :type text)
        (:headline_offset :desc "file offset of the headline with this tag"
                          :type integer
                        :constraints (notnull))
        (:tag :desc "the text value of this tag"
              :type text)
        (:is_inherited :desc "true if this tag is inherited"
                    :type boolean
                    :constraints (notnull)))
       (constraints
        (primary :keys (:file_path nil :headline_offset nil :tag nil :is_inherited nil))
        (foreign :ref headlines
                 :keys (:file_path :headline_offset)
                 :parent-keys (:file_path :headline_offset)
                 :on_delete cascade
                 :on_update cascade)))

      (properties
       (desc . "Each row stores one property")
       (columns
        (:file_path :desc "path to the file containing this property"
                    :type text)
        (:headline_offset :desc "file offset of the headline with this property"
                          :type integer
                        :constraints (notnull))
        (:property_offset :desc "file offset of this property in the org file"
                          :type integer)
        (:key_text :desc "this property's key"
                   :type text
                   :constraints (notnull))
        (:val_text :desc "this property's value"
                   :type text
                   :constraints (notnull))
        (:is_inherited :desc "true if this property is inherited (currently unused)"
                    :type boolean
                   :constraints (notnull)))
       (constraints
        (primary :keys (:file_path asc :property_offset asc))
        (foreign :ref headlines
                 :keys (:file_path :headline_offset)
                 :parent-keys (:file_path :headline_offset)
                 :on_delete cascade
                 :on_update cascade)))

      (clocks
       (desc . "Each row stores one clock entry")
       (columns
        (:file_path :desc "path to the file containing this clock"
                    :type text)
        (:headline_offset :desc "offset of the headline with this clock"
                         :type integer
                        :constraints (notnull))
        (:clock_offset :desc "file offset of this clock"
                       :type integer)
        (:time_start :desc "timestamp for the start of this clock"
                     :type integer)
        (:time_end :desc "timestamp for the end of this clock"
                   :type integer)
        (:clock_note :desc "the note entry beneath this clock"
                     :type text))
       (constraints
        (primary :keys (:file_path asc :clock_offset asc))
        (foreign :ref headlines
                 :keys (:file_path :headline_offset)
                 :parent-keys (:file_path :headline_offset)
                 :on_delete cascade
                 :on_update cascade)))

      (logbook_entries
       (desc . "Each row stores one logbook entry (except for clocks)")
       (columns
        (:file_path :desc "path to the file containing this entry"
                    :type text)
        (:headline_offset :desc "offset of the headline with this entry"
                          :type integer
                        :constraints (notnull))
        (:entry_offset :desc "offset of this logbook entry"
                       :type integer)
        (:entry_type :desc "type of this entry (see `org-log-note-headlines')"
                     :type text)
        (:time_logged :desc "timestamp for when this entry was taken"
                      :type integer)
        (:header :desc "the first line of this entry (usually standardized)"
                 :type text)
        (:note :desc "the text of this entry underneath the header"
               :type text))
       (constraints
        (primary :keys (:file_path asc :entry_offset asc))
        (foreign :ref headlines
                 :keys (:file_path :headline_offset)
                 :parent-keys (:file_path :headline_offset)
                 :on_delete cascade
                 :on_update cascade)))

      (state_changes
       (desc . "Each row stores additional metadata for a state change logbook entry")
       (columns
        (:file_path :desc "path to the file containing this entry"
                    :type text)
        (:entry_offset :desc "offset of the logbook entry for this state change"
                       :type integer)
        (:state_old :desc "former todo state keyword"
                    :type text
                    :constraints (notnull))
        (:state_new :desc "updated todo state keyword"
                    :type text
                    :constraints (notnull)))
       (constraints
        (primary :keys (:file_path asc :entry_offset asc))
        (foreign :ref logbook_entries
                 :keys (:file_path :entry_offset)
                 :parent-keys (:file_path :entry_offset)
                 :on_delete cascade
                 :on_update cascade)))

      (planning_changes
       (desc . "Each row stores additional metadata for a planning change logbook entry")
       (columns
        (:file_path :desc "path to the file containing this entry"
                    :type text)
        (:entry_offset :desc "offset of the logbook entry for this planning change"
                       :type integer)
        (:timestamp_offset :desc "offset of the former timestamp"
                           :type integer
                           :constraints (notnull)))
       (constraints
        (primary :keys (:file_path asc :entry_offset asc))
        (foreign :ref timestamps
                 :keys (:file_path :timestamp_offset)
                 :parent-keys (:file_path :timestamp_offset)
                 :on_delete cascade
                 :on_update cascade)
        (foreign :ref logbook_entries
                 :keys (:file_path :entry_offset)
                 :parent-keys (:file_path :entry_offset)
                 :on_delete cascade
                 :on_update cascade)))

      (links
       (desc . "Each rows stores one link")
       (columns
        (:file_path :desc "path to the file containing this link"
                    :type text)
        (:headline_offset :desc "offset of the headline with this link"
                          :type integer
                          :constraints (notnull))
        (:link_offset :desc "file offset of this link"
                      :type integer)
        (:link_path :desc "target of this link (eg url, file path, etc)"
                    :type text
                    :constraints (notnull))
        (:link_text :desc "text of this link"
                    :type text)
        (:link_type :desc "type of this link (eg http, mu4e, file, etc)"
                    :type text
                    :constraints (notnull)))
       (constraints
        (primary :keys (:file_path asc :link_offset asc))
        (foreign :ref headlines
                 :keys (:file_path :headline_offset)
                 :parent-keys (:file_path :headline_offset)
                 :on_delete cascade
                 :on_update cascade)))

      (timestamps
       (desc . "Each row stores one timestamp")
       (columns
        (:file_path :desc "path to the file containing this timestamp"
                    :type text)
        (:headline_offset :desc "offset of the headline containing this timestamp"
                          :type integer
                          :constraints (notnull))
        (:timestamp_offset :desc "offset of this timestamp"
                           :type integer)
        (:raw_value :desc "text representation of this timestamp"
                    :type text
                    :constraints (notnull))
        (:is_active :desc "true if the timestamp is active"
                    :type boolean
                    :constraints (notnull))
        (:warning_type :desc "warning type of this timestamp"
                       :type text
                       :allowed (all first))
        (:warning_value :desc "warning shift of this timestamp"
                        :type integer)
        (:warning_unit :desc "warning unit of this timestamp "
                       :type text
                       :allowed (hour day week month year))
        (:repeat_type :desc "repeater type of this timestamp"
                      :type text
                      :allowed (catch-up restart cumulate))
        (:repeat_value :desc "repeater shift of this timestamp"
                       :type integer)
        (:repeat_unit :desc "repeater unit of this timestamp"
                      :type text
                      :allowed (hour day week month year))
        (:time_start :desc "the start time (or only time) of this timestamp"
                     :type integer
                     :constraints (notnull))
        (:time_end :desc "the end time of this timestamp"
                   :type integer)
        (:start_is_long :desc "true if the start time is in long format"
                        :type boolean
                        :constraints (notnull))
        (:end_is_long :desc "true if the end time is in long format"
                      :type boolean))
       (constraints
        (primary :keys (:file_path asc :timestamp_offset asc))
        (foreign :ref headlines
                 :keys (:file_path :headline_offset)
                 :parent-keys (:file_path :headline_offset)
                 :on_delete cascade
                 :on_update cascade))))
      "Internal schema representation as a pure symbolic list."))

(defun org-sql--meta-format-column-constraints (constraints-meta)
  "Return formatted column constraints for CONSTRAINTS-META."
  (cl-flet
      ((format-constraint
        (constraint)
        (pcase constraint
          ('notnull "NOT NULL")
          ('unique "UNIQUE")
          ;; TODO add CHECK?
          ;; TODO add PRIMARY KEY?
          (e (error "Unknown constraint %s" e)))))
    (->> constraints-meta
         (-map #'format-constraint)
         (s-join " "))))

(defun org-sql--meta-format-columns (columns-meta)
  "Return formatted tables columns for COLUMNS-META."
  (cl-flet
      ((format-column
        (column-meta)
        (-let* (((name . meta) column-meta)
                (name* (org-sql--kw-to-colname name))
                ((&plist :type :constraints) meta)
                (type* (upcase (symbol-name type)))
                (column-str (format "%s %s" name* type*)))
          (if (not constraints) column-str
            (->> (org-sql--meta-format-column-constraints constraints)
                 (format "%s %s" column-str))))))
    (-map #'format-column columns-meta)))

(defun org-sql--meta-format-table-constraints (constraints-meta)
  "Return formatted table constraints for CONSTRAINTS-META."
  (cl-labels
      ((format-primary
        (meta)
        (-let* (((&plist :keys) meta))
          (->> (-partition 2 keys)
               (--map (let ((n (org-sql--kw-to-colname (car it)))
                            (s (-some-> (cadr it) (symbol-name) (upcase))))
                        (if s (format "%s %s" n s) n)))
               (s-join ",")
               (format "PRIMARY KEY (%s)"))))
       (format-foreign
        (meta)
        (-let* (((&plist :ref :keys :parent-keys :on_delete :on_update) meta)
                (keys* (->> keys (-map #'org-sql--kw-to-colname) (s-join ",")))
                (parent-keys* (->> parent-keys
                                   (-map #'org-sql--kw-to-colname)
                                   (s-join ",")))
                (foreign-str (format "FOREIGN KEY (%s) REFERENCES %s (%s)"
                                     keys* ref parent-keys*))
                (on-delete* (-some->> on_delete
                              (symbol-name)
                              (upcase)
                              (format "ON DELETE %s")))
                (on-update* (-some->> on_update
                              (symbol-name)
                              (upcase)
                              (format "ON UPDATE %s"))))
          (->> (list foreign-str on-delete* on-update*)
               (-non-nil)
               (s-join " "))))
       (format-constraint
        (constraint)
        (pcase constraint
          (`(primary . ,meta) (format-primary meta))
          (`(foreign . ,meta) (format-foreign meta)))))
    (-map #'format-constraint constraints-meta)))

(defun org-sql--meta-create-table (tbl-meta)
  "Return formatted 'CREATE TABLE' SQL for TBL-META."
  (-let* (((tbl-name . meta) tbl-meta)
          ((&alist 'columns 'constraints) meta)
          (column-str (->> (org-sql--meta-format-table-constraints constraints)
                           (append (org-sql--meta-format-columns columns))
                           (s-join ","))))
    (format "CREATE TABLE %s (%s);" tbl-name column-str)))

;; TODO, make a formating function to convert a lisp obj to schema
(defconst org-sql--schemas
  '("CREATE TABLE files (file_path TEXT PRIMARY KEY ASC,md5 TEXT NOT NULL,size INTEGER NOT NULL,time_modified INTEGER,time_created INTEGER,time_accessed INTEGER);"
    "CREATE TABLE headlines (file_path TEXT, headline_offset INTEGER, tree_path TEXT, headline_text TEXT NOT NULL, keyword TEXT, effort INTEGER, scheduled_offset INTEGER, deadline_offset INTEGER, closed_offset INTEGER, priority CHAR, archived BOOLEAN, commented BOOLEAN, content TEXT, PRIMARY KEY (file_path ASC, headline_offset ASC), FOREIGN KEY (file_path) REFERENCES files (file_path) ON UPDATE CASCADE ON DELETE CASCADE);"
    "CREATE TABLE tags (file_path TEXT,headline_offset INTEGER,tag TEXT,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path, headline_offset, tag, inherited));"
    "CREATE TABLE properties (file_path TEXT,headline_offset INTEGER,property_offset INTEGER,key_text TEXT NOT NULL,val_text TEXT NOT NULL,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, property_offset ASC));"
    "CREATE TABLE clocks (file_path TEXT,headline_offset INTEGER,clock_offset INTEGER,time_start INTEGER,time_end INTEGER,clock_note TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset)ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, clock_offset ASC));"
    "CREATE TABLE logbook_entries (file_path TEXT,headline_offset INTEGER,entry_offset INTEGER,entry_type TEXT,time_logged INTEGER,header TEXT,note TEXT,FOREIGN KEY (file_path, headline_offset)REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
    "CREATE TABLE state_changes (file_path TEXT,entry_offset INTEGER,state_old TEXT NOT NULL,state_new TEXT NOT NULL,FOREIGN KEY (file_path, entry_offset) REFERENCES logbook_entries (file_path, entry_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
    "CREATE TABLE planning_changes (file_path TEXT, entry_offset INTEGER, timestamp_offset INTEGER NOT NULL, FOREIGN KEY (file_path, entry_offset) REFERENCES logbook_entries (file_path, entry_offset) ON DELETE CASCADE ON UPDATE CASCADE, PRIMARY KEY (file_path ASC, entry_offset ASC), FOREIGN KEY (file_path, timestamp_offset) REFERENCES timestamp (file_path, timestamp_offset) ON DELETE CASCADE ON UPDATE CASCADE);"
    "CREATE TABLE links (file_path TEXT,headline_offset INTEGER,link_offset INTEGER,link_path TEXT,link_text TEXT,link_type TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, link_offset ASC));"
    "CREATE TABLE timestamps (file_path TEXT, headline_offset INTEGER, timestamp_offset INTEGER, raw_value TEXT NOT NULL, type TEXT, warning_type TEXT, warning_value INTEGER, warning_unit TEXT, repeat_type TEXT, repeat_value INTEGER, repeat_unit TEXT, time INTEGER NOT NULL, time_end INTEGER, resolution TEXT, resolution_end TEXT, PRIMARY KEY (file_path, timestamp_offset), FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON DELETE CASCADE ON UPDATE CASCADE);")
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
  '(clock done state note reschedule delschedule redeadline deldeadline refile)
  "List of logbook entry types to include in the database.
List members are any of the keys from `org-log-note-headings' with the
exception of 'clock-out' as these are treated as clock-notes (see
`org-sql-store-clock-notes'). To include none set to nil."
  :type '(set :tag "List of types to include"
              (const :tag "Clocks" clock)
              (const :tag "Closing notes" done)
              (const :tag "State changes" state)
              (const :tag "Notes taken" note)
              (const :tag "Rescheduled tasks" reschedule)
              (const :tag "Unscheduled tasks" delschedule)
              (const :tag "Redeadlined tasks" redeadline)
              (const :tag "Undeadlined tasks" deldeadline)
              (const :tag "Refiled tasks" refile))
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

;;; helper functions

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
    (->> (s-trim out)
         (s-split "\n")
         (--map (s-split "|" it))
         (--map (-interleave cols it)))))

;;; SQL formatting helper functions

(defun org-sql--escape-text (txt)
  "Escape and quote TXT for insertion into SQL database.
This assumes the insertion command will be run on a shell where the
sql command string is in double quotes."
  (->> (s-replace-regexp "'" "''" txt)
       (s-replace-regexp "\n" "'||char(10)||'")
       (format "'%s'")))

(defun org-sql--to-string (s)
  "Convert S to a string suitable for insertion into SQLite db.
Converts numbers to strings, flanks strings with '\"', and converts
any other symbols to their symbol name."
  (pcase s
    (`nil "NULL")
    ((pred stringp) (org-sql--escape-text s))
    ((pred numberp) (number-to-string s))
    ((pred keywordp) (org-sql--kw-to-colname s))
    ((pred symbolp) (org-sql--escape-text (symbol-name s)))
    (e (error "Cannot convert to string: %s" e))))

;; TODO this name is too specific
;; TODO this now seems redundant
(defun org-sql--kw-to-colname (kw)
  "Return string representation of KW for column in sql database."
  (if (keywordp kw) (--> kw (symbol-name it) (substring it 1))
    (error "Not a keyword: %s" kw)))

(defun org-sql--plist-concat (sep plist)
  "Concatenate a PLIST to string to be used in a SQL statement.
Returns a string formatted like 'prop1 = value1 SEP prop2 = value2'
from a plist like '(:prop1 value1 :prop2 value2)."
  (let ((keys (->> (org-sql--plist-get-keys plist)
                   (-map #'org-sql--kw-to-colname)))
        (vals (->> (org-sql--plist-get-vals plist)
                   (-map #'org-sql--to-string))))
    (-some->> (--zip-with (format "%s=%s" it other) keys vals)
      (s-join sep))))

;;; SQL command formatting functions

(defun org-sql--fmt-insert (tbl-name keyvals)
  "Format SQL insert command from TBL-NAME.
KEYVALS is a plist of column names as the keys and values as the
target value for each column."
  (let ((columns (->> (org-sql--plist-get-keys keyvals)
                      (-map #'org-sql--kw-to-colname)
                      (s-join ",")))
        (values (->> (org-sql--plist-get-vals keyvals)
                     (-map #'org-sql--to-string)
                     (s-join ","))))
    (format "insert into %s (%s) values (%s);" tbl-name columns values)))

(defun org-sql--fmt-update (tbl-name set where)
  "Format SQL update command from TBL-NAME.
SET is a plist of values to set and WHERE is a plist of conditions
to be satisfied (using and)."
  (let ((set* (org-sql--plist-concat "," set))
        (where* (org-sql--plist-concat " and " where )))
    (format "update %s set %s where %s;" tbl-name set* where*)))

(defun org-sql--fmt-delete (tbl-name where)
  "Format SQL update command from TBL-NAME and WHERE."
  (->> (org-sql--plist-concat " and " where)
       (format "delete from %s where %s;" tbl-name)))

(defun org-sql--fmt-delete-all (tbl-name)
  "Return SQL command to delete everything in TBL-NAME."
  (format "delete from %s;" tbl-name))

(defun org-sql--fmt-select (tbl-name columns)
  "Return SQL command to select COLUMNS in TBL-NAME."
  (let ((columns* (or (-some->> (-map #'org-sql--kw-to-colname columns)
                        (s-join ","))
                      "*")))
    (format "select %s from %s;" columns* tbl-name)))

(defun org-sql--pragma-merge-default (pragma)
  "Add PRAGMA to `org-sql--default-pragma'."
  (if (not pragma) org-sql--default-pragma
    (cl-flet
        ((getv
          (p)
          (or (plist-get org-sql--default-pragma p)
              (plist-get pragma p))))
      (->> (org-sql--plist-get-keys org-sql--default-pragma)
           (append (org-sql--plist-get-keys pragma))
           (-uniq)
           (--mapcat (list it (getv it)))))))

(defun org-sql--fmt-pragma (plist)
  "Create a SQL statement for setting pragmas in PLIST.
PLIST contains the pragmas as the properties and their intended
values as the property values."
  (let ((pragmas (->> (org-sql--plist-get-keys plist)
                      (-map #'org-sql--kw-to-colname))))
    (->> (org-sql--plist-get-vals plist)
         (--zip-with (format "PRAGMA %s=%s;" it other) pragmas)
         (s-join ""))))

(defun org-sql--fmt-trans (sql-strs)
  "Format SQL transactions string.
SQL-STRS is a list of individual SQL commands to be included in the
transaction."
  ;; turn on deferred keys for all transactions
  (let ((pragma (->> (org-sql--pragma-merge-default '(:defer_foreign_keys on))
                     (org-sql--fmt-pragma))))
  (-some->> sql-strs
    (s-join "")
    (format "begin transaction;%scommit;")
    (concat pragma))))

;;; SQL command abstractions

;; TODO don't hardcode the exe paths or the tmp path...just to make everyone happy
(defun org-sql--cmd (db-path sql-cmd)
  "Execute SQL-CMD using sqlite3 on database at DB-PATH."
  (let ((cmd (->> (s-replace "'" "'\"'\"'" sql-cmd)
                  (format "/usr/bin/sqlite3 %s '%s'" db-path))))
    (shell-command-to-string cmd)))

(defun org-sql--cmd* (db-path sql-cmd)
  "Execute SQL-CMD using sqlite3 on database at DB-PATH.
This has the same effect as `org-sql--cmd' except that SQL-CMD is
saved to a temp file and redirected into the sqlite command, and
thus is suitable for large strings that would otherwise surpass
shell limits."
  (let* ((tmp-path (format "/tmp/org-sql-cmd-%s" (round (float-time))))
         (cmd (format "/usr/bin/sqlite3 %s < %s" db-path tmp-path)))
    (f-write sql-cmd 'utf-8 tmp-path)
    (let ((res (shell-command-to-string cmd)))
      (f-delete tmp-path)
      res)))

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
        (-let* (((head rest*) (-split-at lb-index children))
                ((break . rest) rest*)
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
      (if (not (org-ml-is-type 'paragraph first)) (cons nil rest)
        (-let (((p0 . p1) (org-sql--split-paragraph first)))
          (if (not p0) `(,p1 . ,rest) `(,p0 . (,p1 . ,rest))))))))

;; TODO this docstring sucks
(defun org-sql--get-header-substring (entry key)
  "Return header substring for ENTRY.
KEY is a valid key for the entry."
  (-let* ((e (cdr entry))
          ((&plist :header-text) e)
          ((begin . end) (plist-get e key)))
    (substring header-text begin end)))

;; TODO this docstring sucks
(defun org-sql--get-header-timestamp (entry key)
  "Return header timestamp for ENTRY.
KEY is a valid key for the entry."
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
          ;; NOTE if clocks are malformed they may not have a start time
          :state-old (-some-> ts
                       (org-ml-timestamp-get-start-time)
                       (org-ml-build-timestamp!))
          :state-new (-some-> ts
                       (org-ml-timestamp-get-end-time)
                       (org-ml-build-timestamp!))
          :note-text nil)))

(defun org-sql--flatten-lb-entries (children)
  "Return logbook drawer CHILDREN as flattened list."
  (cl-labels
      ((add-note
        (clock-entry note)
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
;; These are functions used to pull data from the org-data tree given by
;; `org-element-parse-buffer'. They all adhere to the same idiom where they take
;; an accumulator as the first argument and return a modified accumulator with
;; the data to be added to the database. The accumulator is a list of typed
;; plist, where the car is the table name, the properties are the column names,
;; and the values are the values to be inserted into those columns.

(defmacro org-sql--cons (acc tbl-name &rest plist)
  "Add line to ACC under TBL-NAME for PLIST."
  (declare (indent 2))
  (let ((valid-keys (->> org-sql--metaschema
                         (alist-get tbl-name)
                         (alist-get 'columns)
                         (-map #'car)))
        (input-keys (->> (-partition 2 plist)
                         (-map #'car))))
    (unless valid-keys (error "Invalid table name: %s" tbl-name))
    (-some->> (-difference valid-keys input-keys)
      (error "Keys not given for table %s: %s" tbl-name))
    (-some->> (-difference input-keys valid-keys)
      (error "Keys not valid for table %s: %s" tbl-name))
    `(cons (list ',tbl-name ,@plist) ,acc)))

(defun org-sql--extract (acc fun objs &rest args)
  "Iterate through OBJS and add them to accumulator ACC with FUN.
FUN is a function that takes a single object from OBJS, the accumulator,
and ARGS. FUN adds OBJ to ACC and returns new ACC."
  (--reduce-from (apply fun acc it args) acc objs))

(defun org-sql--extract-lb-clock (acc entry headline fp)
  "Add data from logbook clock ENTRY to accumulator ACC."
  (-let (((&plist :offset :note-text) (cdr entry))
         ((&plist :state-old start :state-new end) (cdr entry)))
    (org-sql--cons acc clocks
      :file_path fp
      :headline_offset (org-element-property :begin headline)
      :clock_offset offset
      :time_start (-some-> start
                    (org-ml-timestamp-get-start-time)
                    (org-ml-time-to-unixtime))
      :time_end (-some-> end
                  (org-ml-timestamp-get-start-time)
                  (org-ml-time-to-unixtime))
      :clock_note (when org-sql-store-clock-notes note-text))))

(defun org-sql--extract-lb-item (acc entry headline fp)
  "Add general logbook ENTRY to ACC."
  (-let* (((entry-type . entry-plist) entry)
          ((&plist :offset :header-text :note-text) entry-plist))
    (org-sql--cons acc logbook_entries
      :file_path fp
      :headline_offset (org-ml-get-property :begin headline)
      :entry_offset offset
      :entry_type entry-type
      :time_logged (-some->> (org-sql--get-header-timestamp entry :ts)
                     (org-ml-timestamp-get-start-time)
                     (org-ml-time-to-unixtime))
      :header header-text
      :note note-text)))

(defun org-sql--extract-lb-state-change (acc entry headline fp)
  "Add data from state-change logbook ENTRY to accumulator ACC."
  (-> (org-sql--extract-lb-item acc entry headline fp)
      (org-sql--cons state_changes
          :file_path fp
          :entry_offset (plist-get (cdr entry) :offset)
          :state_old (org-sql--get-header-substring entry :old-state)
          :state_new (org-sql--get-header-substring entry :new-state))))

(defun org-sql--extract-lb-planning-change (acc entry headline fp)
  "Add data from planning-change logbook ENTRY to accumulator ACC."
  (let ((ts (org-sql--get-header-timestamp entry :old-state)))
    (-> (org-sql--extract-lb-item acc entry headline fp)
        (org-sql--cons planning_changes
            :file_path fp
            :entry_offset (plist-get (cdr entry) :offset)
            :timestamp_offset (org-ml-get-property :begin ts))
        (org-sql--extract-ts ts headline fp))))
         
(defun org-sql--extract-logbook (acc headline fp)
  "Given HEADLINE, find logbook drawer and add to accumulator ACC."
  (cl-flet
      ((extract-entry
        (acc entry)
        (let ((entry-type (car entry)))
          (if (not (memq entry-type org-sql-included-logbook-types)) acc
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
  "Add properties data from HEADLINE to accumulator ACC."
  (if (eq 'all org-sql-ignored-properties) acc
    (let ((node-props
           (->> (org-ml-headline-get-node-properties headline)
                (--remove (member (org-ml-get-property :key it)
                                  (append org-sql--ignored-properties-default
                                          org-sql-ignored-properties))))))
      (cl-flet
          ((from
            (acc np)
            (org-sql--cons acc properties
              :file_path fp
              :headline_offset (org-ml-get-property :begin headline)
              :property_offset (org-ml-get-property :begin np)
              :key_text (org-ml-get-property :key np)
              :val_text (org-ml-get-property :value np)
              ;; TODO add inherited flag
              :is_inherited nil)))
        (org-sql--extract acc #'from node-props)))))

(defun org-sql--extract-tags (acc headline fp)
  "Extract tags data from HEADLINE and add to accumulator ACC."
  (if (eq 'all org-sql-ignored-tags) acc
    (cl-flet
        ((from
          (acc tag inherited)
          (org-sql--cons acc tags
            :file_path fp
            :headline_offset (org-ml-get-property :begin headline)
            :tag tag
            :is_inherited inherited))
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
            (org-sql--cons acc links
              :file_path fp
              :headline_offset (org-ml-get-property :begin headline)
              :link_offset (org-ml-get-property :begin link)
              :link_path (org-ml-get-property :path link)
              :link_text (->> (org-ml-get-children link)
                              (-map #'org-ml-to-string)
                              (s-join ""))
              :link_type (org-ml-get-property :type link))))
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
        (when time (org-ml--time-is-long time))))
    (let ((start (org-ml-timestamp-get-start-time ts))
          (end (org-ml-timestamp-get-end-time ts)))
      (org-sql--cons acc timestamps
        :file_path fp
        :headline_offset (org-ml-get-property :begin headline)
        :timestamp_offset (org-ml-get-property :begin ts)
        :is_active (org-ml-timestamp-is-active ts)
        :warning_type (org-ml-get-property :warning-type ts)
        :warning_value (org-ml-get-property :warning-value ts)
        :warning_unit (org-ml-get-property :warning-unit ts)
        :repeat_type (org-ml-get-property :repeater-type ts)
        :repeat_value (org-ml-get-property :repeater-value ts)
        :repeat_unit (org-ml-get-property :repeater-unit ts)
        :time_start (org-ml-time-to-unixtime start)
        :start_is_long (get-resolution start)
        :time_end (-some-> end (org-ml-time-to-unixtime))
        :end_is_long (get-resolution end)
        :raw_value (org-ml-get-property :raw-value ts)))))

(defun org-sql--extract-hl-contents (acc headline fp)
  "Add contents from partitioned header HEADLINE to accumulator ACC."
  ;; TODO this only works when `org-log-into-drawer' is defined
  (-if-let (pattern (-some--> org-sql-included-contents-timestamp-types
                      (--map `(:type ',it) it)
                      `(:any * (:and timestamp (:or ,@it)))))
      (let ((timestamps
             (-some->> (org-ml-headline-get-section headline)
               ;; TODO need a function in org-ml that returns non-meta
               (--remove (org-ml-is-any-type '(planning property-drawer) it))
               (--remove (equal (org-element-property :drawer-name it)
                                org-log-into-drawer))
               (org-ml-match pattern))))
        (org-sql--extract acc #'org-sql--extract-ts timestamps headline fp))
    acc))

(defun org-sql--extract-hl-meta (acc headline fp)
  "Add general data from HEADLINE to accumulator ACC."
  (-let* (((&plist :closed :scheduled :deadline)
           (->> (org-ml-headline-get-planning headline)
                ;; TODO make this function public
                (org-ml--get-all-properties)))
          (planning-timestamps (-non-nil (list scheduled deadline closed))))
    (-> (org-sql--cons acc headlines
          :file_path fp
          :headline_offset (org-ml-get-property :begin headline)
          :tree_path (org-sql--headline-get-path headline)
          :headline_text (org-ml-get-property :raw-value headline)
          :keyword (org-ml-get-property :todo-keyword headline)
          :effort (-some-> (org-ml-headline-get-node-property "Effort" headline)
                    (org-sql--effort-to-int))
          :scheduled_offset (-some->> scheduled (org-ml-get-property :begin))
          :deadline_offset (-some->> deadline (org-ml-get-property :begin))
          :closed_offset (-some->> closed (org-ml-get-property :begin))
          :priority (-some->> (org-ml-get-property :priority headline)
                      (byte-to-string))
          :is_archived (org-ml-get-property :archivedp headline)
          :is_commented (org-ml-get-property :commentedp headline)
          :content nil)
        (org-sql--extract #'org-sql--extract-ts planning-timestamps headline fp)
        (org-sql--extract-hl-contents headline fp))))

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

(defun org-sql--extract-file (fp md5 acc)
  "Extract the file at FP.
MD5 is the md5sum of the file. The results are accumulated in ACC
which is returned on exit."
  (let ((fsize (file-attribute-size (file-attributes fp))))
    (with-current-buffer (find-file-noselect fp t)
      (-> (org-sql--cons acc files
            :file_path fp
            :md5 md5
            :size fsize)
          (org-sql--extract-buffer fp)))))

;;; database syncing functions

(defun org-sql--get-inserts (actions format-fun)
  "Return formatted SQL insert commands.
ACTIONS is a list of file actions to insert, and FORMAT-FUN is
the function used to format the final SQL output."
  (cl-flet
      ((cons-insert
        (acc action)
        (-let (((&plist :disk-path :hash) action))
          (org-sql--extract-file disk-path hash acc))))
    (let ((acc))
      (->> (-reduce-from #'cons-insert acc actions)
           (--map (funcall format-fun (car it) (cdr it)))))))

(defun org-sql--get-updates (actions format-fun)
  "Return formatted SQL update commands.
ACTIONS is a list of file actions to update, and FORMAT-FUN is
the function used to format the final SQL output."
  (cl-flet
      ((fmt-update
        (action)
        (-let (((&plist :disk-path :hash) action))
          ;; TODO add compile time check for this
          (funcall format-fun 'files `(:file_path ,disk-path) `(:md5 ,hash)))))
    (-map #'fmt-update actions)))
           
(defun org-sql--get-deletes (actions format-fun)
  "Return formatted SQL delete commands.
ACTIONS is a list of file actions to delete, and FORMAT-FUN is
the function used to format the final SQL output."
  (cl-flet
      ((fmt-update
        (action)
        (-let (((&plist :db-path) action))
          ;; TODO add compile time check for this
          (funcall format-fun 'files `(:file_path ,db-path)))))
    (-map #'fmt-update actions)))

(defun org-sql--classify-transactions (on-disk in-db)
  "Return a list of classified file actions.
ON-DISK and IN-DB are lists of file cells where each member is like
\(md5 . filepath). Return an alist where the keys represent the
actions to take on the files on disk/in the database. The keys of
the alist will be 'noops', 'inserts', 'updates', and 'deletes'."
  (cl-flet
      ((get-path
        (key alist)
        (alist-get key alist nil nil #'equal))
       (classify-transaction
        (transaction)
        (-let (((&plist :disk-path :db-path) transaction))
          ;; for a given md5, check the corresponding path given for its disk
          ;; location and in the db to determine the action to take
          (cond
           ;; if paths are equal, do nothing
           ((equal disk-path db-path) 'noops)
           ;; if paths non-nil but unequal, assume disk path changed and update
           ((and disk-path db-path) 'updates)
           ;; if path on in db doesn't exist, assume new file and insert
           ((and disk-path (not db-path) 'inserts))
           ;; if path on on disk doesn't exist, assume removed file and delete
           ((and (not disk-path) db-path) 'deletes)
           ;; at least one path should be non-nil, else there is a problem
           (t (error "Transaction classifier: this should not happen"))))))
    (->> (-union (-map #'car on-disk) (-map #'car in-db))
         (--map (list :hash it
                      :disk-path (get-path it on-disk)
                      :db-path (get-path it in-db)))
         (-group-by #'classify-transaction))))

(defun org-sql--files-on-disk ()
  "Return alist for file paths in `org-sql-files'.
In each cell, the car is the file path and cdr is the file's MD5."
  (cl-flet
      ((cons-md5
         (fp)
         (let ((md5 (->> (format "md5sum %s" fp)
                         (shell-command-to-string)
                         (s-split " ")
                         (car))))
           (cons md5 fp)))
       (expand-if-dir
        (fp)
        (if (not (file-directory-p fp)) `(,fp)
            (directory-files fp t "\\`.*\\.org\\(_archive\\)?\\'"))))
    (if (stringp org-sql-files)
        (error "`org-sql-files' must be a list of paths")
      (->> (-mapcat #'expand-if-dir org-sql-files)
           (-filter #'file-exists-p)
           (-map #'cons-md5)))))

(defun org-sql--files-in-db ()
  "Get all files and their metadata from the database."
  (when (file-exists-p org-sql-sqlite-path)
    (let* ((columns '(:md5 :file_path))
           (sql-select (org-sql--fmt-select 'files columns)))
      (--> (org-sql--cmd org-sql-sqlite-path sql-select)
           (org-sql--to-plist it columns)
           (-map #'org-sql--plist-get-vals it)
           (--map (cons (car it) (cadr it)) it)))))

(defun org-sql--get-transactions ()
  "Return plist of the transactions to be performed on the db.
The plist has three properties (delete, insert, update) for the three
type of commands that are performed on the database during an update.
If NEWLINES is t, add newlines between SQL commands; this is useful
for dumping to buffers."
  (-let* ((on-disk (org-sql--files-on-disk))
          (in-db (org-sql--files-in-db))
          ((&alist 'updates 'inserts 'deletes)
           (org-sql--classify-transactions on-disk in-db)))
    (->> (append (org-sql--get-inserts inserts #'org-sql--fmt-insert)
                 (org-sql--get-updates updates #'org-sql--fmt-update)
                 (org-sql--get-deletes deletes #'org-sql--fmt-delete))
         (reverse)
         (org-sql--fmt-trans))))

(defun org-sql-dump-update-transactions ()
  "Dump the transactions to be committed the database during an update.

It will have three sections denoted \"### DELETE ###\", \" ###
UPDATE ###\", and \"### INSERT ###\". Note this function is only
useful for debugging where one wants to see the exact
transactions to be committed and/or save a file to run the SQL
commands outside of this package."
  (interactive)
  (let ((out (org-sql--get-transactions)))
    (switch-to-buffer "SQL: Org-update-dump")
    (insert (s-replace ";" ";\n" out))))

(defun org-sql-init-db ()
  "Add schemas to database if they do not exist already.
This assumes an active connection is open."
  ;; assume that the db will be created when a new connection is opened
  (org-sql--cmd org-sql-sqlite-path (s-join "" org-sql--schemas)))

(defun org-sql-delete-db ()
  "Deletes the database from disk."
  (when (file-exists-p org-sql-sqlite-path)
    (delete-file org-sql-sqlite-path org-sql-sqlite-path)))

(defun org-sql-update-db ()
  "Update the database."
  (org-sql--cmd* org-sql-sqlite-path (org-sql--get-transactions)))

(defun org-sql-clear-db ()
  "Clear the database."
  ;; only delete from files as we assume actions here cascade down
  (org-sql--cmd org-sql-sqlite-path (org-sql--fmt-delete-all 'files)))

;;; interactive user functions

(defun org-sql-user-update ()
  "Update the Org SQL database."
  (interactive)
  ;; TODO need to see if schema is correct?
  (message "Updating Org SQL database")
  (let ((out (org-sql-update-db)))
    (when org-sql-debug
      (message "Debug output for org-sql update")
      (message (if (equal out "") "Run Successfully" out))))
  (message "Org SQL update complete"))

(defun org-sql-user-clear-all ()
  "Remove all entries in the database."
  (interactive)
  (if (y-or-n-p "Really clear all? ")
      (progn
        (message "Clearing Org SQL database")
        (let ((out (org-sql-clear-db)))
          (when org-sql-debug
            (message "Debug output for org-sql clear-all")
            (message (if (equal out "") "Run Successfully" out))))
        (message "Org SQL clear completed"))
    (message "Aborted")))

(defun org-sql-user-reset ()
  "Reset the database with default schema."
  (interactive)
  (if (or (not (file-exists-p org-sql-sqlite-path))
          (y-or-n-p "Really reset database? "))
      (progn
        (org-sql-delete-db)
        (message "Resetting Org SQL database")
        (let ((out (org-sql-init-db)))
          (when org-sql-debug
            (message "Debug output for org-sql user-reset")
            (message (if (equal out "") "Run Successfully" out))))
        (message "Org SQL reset completed"))
    (message "Aborted")))

(provide 'org-sql)
;;; org-sql.el ends here
