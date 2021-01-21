;;; org-sql.el --- Org-Mode SQL converter -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, data
;; Homepage: https://github.com/ndwarshuis/org-sql
;; Package-Requires: ((emacs "27.1") (s "1.12") (dash "2.17") (org-ml "5.4.3"))
;; Version: 1.1.0

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

;; This code stores org buffers in a variety of SQL databases for use in
;; processing org-mode data outside of Emacs where SQL operations might be more
;; appropriate.

;; The rough process by which this occurs is:
;;  1) query state of org files on disk and in db (if any) and classify
;;     files as 'updates', 'deletes', or 'inserts'
;;    - updates: a file on disk is also in the database but the path on disk has
;;      changed; this is the part that will be updated
;;    - deletes: a file in the db is not on disk; therefore delete from db
;;    - inserts: a file is on disk but not in the db, therefore insert into db
;;    - NOTE: file equality will be assessed using a hash algorithm (eg md5)
;;    - NOTE: in the case that a file on disk has changed and its path is also
;;      in the db, this file will be deleted and reinserted
;;  2) convert the updates/deletes/inserts into meta query language (MQL, an
;;     internal, database agnostic representation of the SQL statements to be
;;     sent)
;;    - inserts will be constructed using `org-element'/`org-ml' from target
;;      files on disk
;;  3) format MQL to database-specific SQL statements
;;  4) send SQL statements to the configured database

;; The code is roughly arranged as follows:
;; - constants
;; - customization variables
;; - stateless functions
;; - stateful IO functions

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 's)
(require 'sql)
(require 'org)
(require 'org-clock)
(require 'org-ml)

;;;
;;; CONSTANTS
;;;

(defconst org-sql--log-note-keys
  '((:user .  "%u")
    (:user-full . "%U")
    (:ts . "%t")
    (:ts-active . "%T")
    (:short-ts . "%d")
    (:short-ts-active . "%D")
    (:old-state . "%S")
    (:new-state . "%s"))
  "Keywords for placeholders used in `org-log-note-headings'.")

(defconst org-sql--log-note-replacements
  (->> (-map #'cdr org-sql--log-note-keys) (--map (cons it it)))
  "A list to simplify placeholders in `org-log-note-headings'.
This is only used in combination with `org-replace-escapes'")

(defconst org-sql--entry-keys
  (append
   (-map #'car org-sql--log-note-keys)
   '(:file-hash :headline-offset :entry-offset :note-text :header-text :old-ts :new-ts))
  "Valid keys that may be used in logbook entry lists.")

(defconst org-sql--ignored-properties-default
  '("ARCHIVE_ITAGS" "Effort")
  "Property keys to be ignored when inserting in properties table.
It is assumed these are used elsewhere and thus it would be redundant
to store them. This is in addition to any properties specifified by
`org-sql-excluded-properties'.")

(defconst org-sql--content-timestamp-types
  '(active active-range inactive inactive-range)
  "Types of timestamps to include in the database.")

;; file_path and tags columns need to have fixed length for certain DBMSs when
;; used as primary keys. file_path is set to 255 since this is the max length
;; of the filepath string for pretty much all filesystems (including the fancy
;; sci-fi ones like ZFS). tags is set to 32 because...well, who would make a tag
;; in org-mode greater than 32 chars (or even 16)?
(defconst org-sql--file-path-varchar-length 255
  "Length of the file_path column varchar type.")

(defconst org-sql--tag-varchar-length 32
  "Length of the tag column varchar type.")

(defconst org-sql--file_hash-char-length 32
  "Length of the file_hash column char type.")

(eval-and-compile
  ;; TODO use UUID type where possible for the hash column
  (defconst org-sql--mql-tables
    `((file_hashes
       (desc . "Each row describes one org file (which may have multiple filepaths)")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:size :desc "size of the org file in bytes"
               :type integer
               :constraints (notnull)))
        ;; (:lines :desc "number of lines in the org file"
        ;;         :type integer))
       (constraints
        (primary :keys (:file_hash))))

      (file_metadata
       (desc . "Each row stores filesystem metadata for one tracked org file")
       (columns
        (:file_path :desc "path to the org file"
                    :type varchar
                    :length ,org-sql--file-path-varchar-length)
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length
                    :constraints (notnull)))
       (constraints
        (primary :keys (:file_path))
        (foreign :ref file_hashes
                 :keys (:file_hash)
                 :parent-keys (:file_hash)
                 ;; TODO this 'on_delete' is not lispy because it has a '_'
                 :on_delete cascade)))

      (headlines
       (desc . "Each row stores one headline in a given org file and its metadata")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:headline_offset :desc "file offset of the headline's first character"
                          :type integer)
        (:headline_text :desc "raw text of the headline"
                        :type text
                        :constraints (notnull))
        (:keyword :desc "the TODO state keyword"
                  :type text)
        (:effort :desc "the value of the Effort property in minutes"
                 :type integer)
        (:priority :desc "character value of the priority"
                   :type text)
        (:is_archived :desc "true if the headline has an archive tag"
                      :type boolean
                      :constraints (notnull))
        (:is_commented :desc "true if the headline has a comment keyword"
                       :type boolean
                       :constraints (notnull))
        (:content :desc "the headline contents"
                  :type text))
       (constraints
        (primary :keys (:file_hash :headline_offset))
        (foreign :ref file_hashes
                 :keys (:file_hash)
                 :parent-keys (:file_hash)
                 :on_delete cascade)))

      (headline_closures
       (desc . "Each row stores the ancestor and depth of a headline relationship (eg closure table)")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:headline_offset :desc "offset of this headline"
                          :type integer)
        (:parent_offset :desc "offset of this headline's parent"
                        :type integer)
        (:depth :desc "levels between this headline and the referred parent"
                :type integer))
       (constraints
        (primary :keys (:file_hash :headline_offset :parent_offset))
        (foreign :ref headlines
                 :keys (:file_hash :headline_offset)
                 :parent-keys (:file_hash :headline_offset)
                 :on_delete cascade)
        (foreign :ref headlines
                 :keys (:file_hash :parent_offset)
                 :parent-keys (:file_hash :headline_offset))))
                 ;; :on_delete no-action)))
                 ;; :on_delete cascade)))

      (timestamps
       (desc . "Each row stores one timestamp")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
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
                       :type enum
                       :allowed (all first))
        (:warning_value :desc "warning shift of this timestamp"
                        :type integer)
        (:warning_unit :desc "warning unit of this timestamp "
                       :type enum
                       :allowed (hour day week month year))
        (:repeat_type :desc "repeater type of this timestamp"
                      :type enum
                      :allowed (catch-up restart cumulate))
        (:repeat_value :desc "repeater shift of this timestamp"
                       :type integer)
        (:repeat_unit :desc "repeater unit of this timestamp"
                      :type enum
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
        (primary :keys (:file_hash :timestamp_offset))
        (foreign :ref headlines
                 :keys (:file_hash :headline_offset)
                 :parent-keys (:file_hash :headline_offset)
                 :on_delete cascade)))

      (planning_entries
       (desc . "Each row stores the metadata for headline planning timestamps.")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:headline_offset :desc "file offset of the headline with this tag"
                          :type integer)
        (:planning_type :desc "the type of this planning entry"
                        :type enum
                        :length 9
                        :allowed (closed scheduled deadline))
        (:timestamp_offset :desc "file offset of this entries timestamp"
                           :type integer
                           :constraints (notnull)))
       (constraints
        (primary :keys (:file_hash :headline_offset :planning_type))
        (foreign :ref timestamps
                 :keys (:file_hash :timestamp_offset)
                 :parent-keys (:file_hash :timestamp_offset)
                 :on_delete cascade)))

      (file_tags
       (desc . "Each row stores one tag at the file level")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:tag :desc "the text value of this tag"
              :type varchar
              :length ,org-sql--tag-varchar-length))
       (constraints
        (primary :keys (:file_hash :tag))
        (foreign :ref file_hashes
                 :keys (:file_hash)
                 :parent-keys (:file_hash)
                 :on_delete cascade)))

      (headline_tags
       (desc . "Each row stores one tag")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:headline_offset :desc "file offset of the headline with this tag"
                          :type integer)
        (:tag :desc "the text value of this tag"
              :type varchar
              :length ,org-sql--tag-varchar-length)
        (:is_inherited :desc "true if this tag is from the ITAGS property"
                       :type boolean
                       :constraints (notnull)))
       (constraints
        (primary :keys (:file_hash :headline_offset :tag :is_inherited))
        (foreign :ref headlines
                 :keys (:file_hash :headline_offset)
                 :parent-keys (:file_hash :headline_offset)
                 :on_delete cascade)))

      (properties
       (desc . "Each row stores one property")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:property_offset :desc "file offset of this property in the org file"
                          :type integer)
        (:key_text :desc "this property's key"
                   :type text
                   :constraints (notnull))
        (:val_text :desc "this property's value"
                   :type text
                   :constraints (notnull)))
       (constraints
        (primary :keys (:file_hash :property_offset))
        (foreign :ref file_hashes
                 :keys (:file_hash)
                 :parent-keys (:file_hash)
                 :on_delete cascade)))

      (file_properties
       (desc . "Each row stores a property at the file level")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:property_offset :desc "file offset of this property in the org file"
                          :type integer))
       (constraints
        (primary :keys (:file_hash :property_offset))
        (foreign :ref file_hashes
                 :keys (:file_hash)
                 :parent-keys (:file_hash)
                 :on_delete cascade)
        (foreign :ref properties
                 :keys (:file_hash :property_offset)
                 :parent-keys (:file_hash :property_offset)
                 :on_delete cascade)))

      (headline_properties
       (desc . "Each row stores a property at the headline level")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:property_offset :desc "file offset of this property in the org file"
                          :type integer)
        (:headline_offset :desc "file offset of the headline with this property"
                          :type integer
                          :constraints (notnull)))
       (constraints
        (primary :keys (:file_hash :property_offset))
        (foreign :ref properties
                 :keys (:file_hash :property_offset)
                 :parent-keys (:file_hash :property_offset)
                 :on_delete cascade)
        (foreign :ref headlines
                 :keys (:file_hash :headline_offset)
                 :parent-keys (:file_hash :headline_offset)
                 :on_delete cascade)))
      
      (clocks
       (desc . "Each row stores one clock entry")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
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
        (primary :keys (:file_hash :clock_offset))
        (foreign :ref headlines
                 :keys (:file_hash :headline_offset)
                 :parent-keys (:file_hash :headline_offset)
                 :on_delete cascade)))

      (logbook_entries
       (desc . "Each row stores one logbook entry (except for clocks)")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
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
        (primary :keys (:file_hash :entry_offset))
        (foreign :ref headlines
                 :keys (:file_hash :headline_offset)
                 :parent-keys (:file_hash :headline_offset)
                 :on_delete cascade)))

      (state_changes
       (desc . "Each row stores additional metadata for a state change logbook entry")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:entry_offset :desc "offset of the logbook entry for this state change"
                       :type integer)
        (:state_old :desc "former todo state keyword"
                    :type text
                    :constraints (notnull))
        (:state_new :desc "updated todo state keyword"
                    :type text
                    :constraints (notnull)))
       (constraints
        (primary :keys (:file_hash :entry_offset))
        (foreign :ref logbook_entries
                 :keys (:file_hash :entry_offset)
                 :parent-keys (:file_hash :entry_offset)
                 :on_delete cascade)))

      (planning_changes
       (desc . "Each row stores additional metadata for a planning change logbook entry")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
        (:entry_offset :desc "offset of the logbook entry for this planning change"
                       :type integer)
        (:timestamp_offset :desc "offset of the former timestamp"
                           :type integer
                           :constraints (notnull)))
       (constraints
        (primary :keys (:file_hash :entry_offset))
        (foreign :ref timestamps
                 :keys (:file_hash :timestamp_offset)
                 :parent-keys (:file_hash :timestamp_offset)
                 :on_delete cascade)
        (foreign :ref logbook_entries
                 :keys (:file_hash :entry_offset)
                 :parent-keys (:file_hash :entry_offset)
                 :on_delete cascade)))

      (links
       (desc . "Each row stores one link")
       (columns
        (:file_hash :desc "hash (MD5) of the org file"
                    :type char
                    :size ,org-sql--file_hash-char-length)
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
        (primary :keys (:file_hash :link_offset))
        (foreign :ref headlines
                 :keys (:file_hash :headline_offset)
                 :parent-keys (:file_hash :headline_offset)
                 :on_delete cascade))))
    "Org-SQL database schema represented in internal meta query
    language (MQL, basically a giant list)"))

(defconst org-sql-table-names (--map (symbol-name (car it)) org-sql--mql-tables)
  "The names of all required tables in org-sql.")

;; TODO what about the windows users?
(defconst org-sql--mysql-exe "mysql"
  "The mysql client command.")

(defconst org-sql--psql-exe "psql"
  "The postgres client command.")

(defconst org-sql--sqlite-exe "sqlite3"
  "The sqlite client command.")

(defconst org-sql--sqlserver-exe "sqlcmd"
  "The sqlserver client command.")

;;;
;;; CUSTOMIZATION OPTIONS
;;;

(defgroup org-sql nil
  "Org mode SQL backend options."
  :tag "Org SQL"
  :group 'org)

;; TODO add sqlite pragma (synchronous and journalmode)
;; TODO add postgres transaction options
(defcustom org-sql-db-config
  (list 'sqlite :path (expand-file-name "org-sql.db" org-directory))
  "Configuration for the org-sql database.

This is a list like (DB-TYPE OPTION-PLIST). The valid keys and
values in OPTION-PLIST depend on the DB-TYPE. Note that all
values in the OPTION-PLIST must be strings.

The following symbols are valid for DB-TYPE:
- `sqlite' (requires the `sqlite3' command)
- `postgres' (requires the `psql', `createdb', and `dropdb'
  commands)

For 'sqlite', the following options are in OPTION-PLIST:
- `:path' (required): the path on disk to use for the database
  file

For 'postgres', the following options are in OPTION-PLIST:
- `:database' (required): the name of the database to use
- `:hostname': the hostname for the database connection
- `:port': the port for the database connection
- `:username': the username for the database connection
- `:password': the password for the database connection (NOTE:
  since setting this option will store the password in plain
  text, consider using a `.pgpass' file\\)"
  ;; TODO add type
  :group 'org-sql)

;; (defcustom org-sql-log-note-headings-overrides nil
;;   "Alist of `org-log-note-headings' for specific files.
;; The car of each cell is the file path, and the cdr is another
;; alist like `org-log-note-headings' that will be used when
;; processing that file. This is useful if some files were created
;; with different patterns for their logbooks as Org-mode itself
;; does not provide any options to control this besides the global
;; `org-log-note-headings'."
;;   :type '(alist :key-type string
;;                 :value-type (alist :key-type symbol
;;                                    :value-type string))
;;   :group 'org-sql)

(defcustom org-sql-files nil
  "A list of org files or directories to put into sql database.
Any directories in this list imply that all files within the
directly are added. Only files ending in .org or .org_archive are
considered. See function `org-sql-files'."
  :type '(repeat :tag "List of files and directories" file)
  :group 'org-sql)

(defcustom org-sql-excluded-properties nil
  "List of properties to exclude from the database.
To exclude all set to 'all' instead of a list of strings."
  :type '(choice
          (const "Ignore All" all)
          (repeat :tag "List of properties to ignore" string))
  :group 'org-sql)

(defcustom org-sql-exclude-inherited-tags nil
  "If t don't include tags in the ARCHIVE_ITAGS property in the database."
  :type 'boolean
  :group 'org-sql)

(defcustom org-sql-excluded-tags nil
  "List of tags to exclude when building the tags table.
To exclude all set to 'all' instead of a list of strings."
  :type '(choice
          (const "Ignore All" all)
          (repeat :tag "List of tags to ignore" string))
  :group 'org-sql)

(defcustom org-sql-excluded-link-types nil
  "List of link types to exclude when building the links table.
Each member should be a string and one of `org-link-types' or
\"file\", \"coderef\", \"custom-id\", \"fuzzy\", or \"id\". See org-element
API documentation or`org-element-link-parser' for details.
To exclude all set to 'all' instead of a list of strings."
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

(defcustom org-sql-excluded-headline-planning-types nil
  "List of headline planning timestamps to exclude in the database.
List members can be ':deadline', ':scheduled', or ':closed'. To
exclude none set to nil."
  :type '(set :tag "List of types to include"
              (const :tag "Deadline Timestamps" :deadline)
              (const :tag "Scheduled Timestamps" :scheduled)
              (const :tag "Closed Timestamps" :closed))
  :group 'org-sql)

(defcustom org-sql-excluded-contents-timestamp-types nil
  "List of timestamp types to exclude from headline content sections.
List members can be the symbols 'active', 'active-range', 'inactive',
or 'inactive-range'. To exclude none set to nil."
  :type '(set :tag "List of types to include"
              (const :tag "Active Timestamps" active)
              (const :tag "Active Timestamp Ranges" active-range)
              (const :tag "Inactive Timestamps" inactive)
              (const :tag "Inactive Timestamp Ranges" inactive-range))
  :group 'org-sql)

(defcustom org-sql-excluded-logbook-types nil
  "List of logbook entry types to exclude from the database.
List members are any of the keys from `org-log-note-headings' with the
exception of 'clock-out' as these are treated as clock-notes (see
`org-sql-exclude-clock-notes'). To include none set to nil."
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

(defcustom org-sql-exclude-clock-notes nil
  "Set to t to store clock notes in the database.
Setting `org-sql-store-clocks' to nil will cause this variable to be
ignored."
  :type 'boolean
  :group 'org-sql)

(defcustom org-sql-debug nil
  "Set to t to enable high-level debugging of SQL transactions."
  :type 'boolean
  :group 'org-sql)

;;;
;;; STATELESS FUNCTIONS
;;;

;;; compile/macro checking

;; case selection statements for sql mode and type

(defun org-sql--sets-equal (list1 list2 &rest args)
  "Return t if LIST1 and LIST2 are equal via set logic.
Either list may contain repeats, in which case nil is returned.
ARGS is a list of additional arguments to pass to `cl-subsetp'."
  (and (equal (length list1) (length list2))
       (apply #'cl-subsetp list1 list2 args)
       (apply #'cl-subsetp list2 list1 args)))

(defmacro org-sql--case-type (type &rest alist-forms)
  "Execute one of ALIST-FORMS depending on TYPE.
TYPE must be one of 'boolean', 'text', 'enum', or 'integer'."
  (declare (indent 1))
  (-let (((keys &as &alist 'boolean 'char 'enum 'integer 'text 'varchar)
          alist-forms))
    (unless (-none? #'null keys)
      (error "Must provide form for all types"))
    `(cl-case ,type
       (boolean ,@boolean)
       (char ,@char)
       (enum ,@enum)
       (integer ,@integer)
       (text ,@text)
       (varchar ,@varchar)
       (t (error "Invalid type: %s" ,type)))))

(defmacro org-sql--case-mode (config &rest alist-forms)
  "Execute one of ALIST-FORMS depending on MODE.
TYPE must be one of 'sqlite' or 'postgres'."
  (declare (indent 1))
  (-let (((keys &as &alist 'mysql 'postgres 'sqlserver 'sqlite)
          (--splice (listp (car it))
                    (-let (((keys . form) it))
                      (--map (cons it form) keys))
                    alist-forms)))
    (unless (-none? #'null keys)
      (error "Must provide form for all modes"))
    `(cl-case (car ,config)
       (mysql ,@mysql)
       (postgres ,@postgres)
       (sqlite ,@sqlite)
       (sqlserver ,@sqlserver)
       (t (error "Invalid mode: %s" (car ,config))))))

(defmacro org-sql--with-config-keys (keys config &rest body)
  "Execute BODY with keys bound to KEYS from CONFIG.
CONFIG is the `org-sql-db-config' list and KEYS are a list of
keys/symbols like those from the &plist switch from `-let'."
  (declare (indent 2))
  `(-let (((&plist ,@keys) (cdr ,config)))
     ,@body))

;; ensure integrity of the metaschema

(eval-when-compile
  (defun org-sql--mql-tables-has-valid-keys (tbl-schema)
    "Verify that TBL-SCHEMA has valid keys in its table constraints."
    (-let* (((tbl-name . (&alist 'constraints 'columns)) tbl-schema)
            (column-names (-map #'car columns)))
      (cl-flet
          ((get-keys
            (constraint)
            (-let (((type . (&plist :keys)) constraint))
              (cons type keys)))
           (test-keys
            (type keys)
            (-some->> (-difference keys column-names)
              (-map #'symbol-name)
              (s-join ", ")
              (error "Mismatched %s keys in table '%s': %s" type tbl-name))))
        (--> (--filter (memq (car it) '(primary foreign)) constraints)
             (-map #'get-keys it)
             (--each it (test-keys (car it) (cdr it)))))))

  (defun org-sql--mql-tables-has-valid-parent-keys (tbl-schema)
    "Verify that TBL-SCHEMA has valid keys in its table foreign constraints."
    (cl-flet
        ((is-valid
          (foreign-meta tbl-name)
          (-let* (((&plist :parent-keys :ref) foreign-meta)
                  (parent-meta (alist-get ref org-sql--mql-tables))
                  (parent-columns (-map #'car (alist-get 'columns parent-meta)))
                  (parent-primary (--> (alist-get 'constraints parent-meta)
                                       (alist-get 'primary it)
                                       (plist-get it :keys))))
            ;; any parent keys must have corresponding columns in the referred
            ;; table
            (-some->> (-difference parent-keys parent-columns)
              (-map #'symbol-name)
              (s-join ", ")
              (error "Mismatched foreign keys between %s and %s: %s" tbl-name ref)))))
      (-let* (((tbl-name . meta) tbl-schema)
              (foreign (->> (alist-get 'constraints meta)
                            (--filter (eq (car it) 'foreign))
                            (-map #'cdr))))
        (--each foreign (is-valid it tbl-name)))))

  (-each org-sql--mql-tables #'org-sql--mql-tables-has-valid-keys)
  (-each org-sql--mql-tables #'org-sql--mql-tables-has-valid-parent-keys))

;; ensure MQL constructors are given the right input

(defun org-sql--mql-check-get-schema-keys (tbl-name)
  "Return a list of columns for TBL-NAME.
The columns are retrieved from `org-sql--mql-tables'."
  (let ((valid-keys (->> org-sql--mql-tables
                         (alist-get tbl-name)
                         (alist-get 'columns)
                         (-map #'car))))
    (unless valid-keys (error "Invalid table name: %s" tbl-name))
    valid-keys))

(defun org-sql--mql-check-columns-all (tbl-name plist)
  "Test if keys in PLIST are valid column names for TBL-NAME.
All column keys must be in PLIST."
  (declare (indent 2))
  (let ((valid-keys (org-sql--mql-check-get-schema-keys tbl-name))
        (input-keys (->> (-partition 2 plist)
                         (-map #'car))))
    (-some->> (-difference valid-keys input-keys)
      (error "Keys not given for table %s: %s" tbl-name))
    (-some->> (-difference input-keys valid-keys)
      (error "Keys not valid for table %s: %s" tbl-name))))

(defun org-sql--mql-check-columns-contains (tbl-name plist)
  "Test if keys in PLIST are valid column names for TBL-NAME.
Only some of the column keys must be in PLIST."
  (declare (indent 2))
  (let ((valid-keys (org-sql--mql-check-get-schema-keys tbl-name))
        (input-keys (->> (-partition 2 plist)
                         (-map #'car))))
    (-some->> (-difference input-keys valid-keys)
      (error "Keys not valid for table %s: %s" tbl-name))))

;;; data constructors

;; meta query language (MQL)

(defmacro org-sql--mql-insert (tbl-name &rest plist)
  "Return an MQL-insert list for TBL-NAME.
PLIST is a property list of the columns and values to insert."
  (declare (indent 1))
  (org-sql--mql-check-columns-all tbl-name plist)
  `(list ',tbl-name ,@plist))

(defmacro org-sql--add-mql-insert (acc tbl-name &rest plist)
  "Add a new MQL-insert list for TBL-NAME to ACC.
PLIST is a property list of the columns and values to insert."
  (declare (indent 2))
  `(cons (org-sql--mql-insert ,tbl-name ,@plist) ,acc))

(defmacro org-sql--mql-update (tbl-name set where)
  "Return an MQL-update list for TBL-NAME.
SET is a plist for the updated values of columns and WHERE is a plist of
columns that must be equal to the values in the plist in order for the update
to be applied."
  (declare (indent 1))
  (org-sql--mql-check-columns-contains tbl-name set)
  (org-sql--mql-check-columns-contains tbl-name where)
  `(list ',tbl-name
         (list 'set ,@set)
         (list 'where ,@where)))

(defmacro org-sql--mql-delete (tbl-name where)
  "Return an MQL-delete list for TBL-NAME.
WHERE is a plist of columns that must be equal to the values in
the plist in order for the delete to be applied."
  (org-sql--mql-check-columns-contains tbl-name where)
  `(list ',tbl-name
         (list 'where ,@where)))

;; TODO I forgot select

;; external state

;; TODO this is hilariously inefficient
(defmacro org-sql--map-plist (key form plist)
  "Return PLIST modified with FORM applied to KEY's value."
  (declare (indent 1))
  `(->> (-partition 2 ,plist)
        (--map-when (eq (car it) ,key)
                    (list (car it) (let ((it (cadr it))) ,form)))
        (-flatten-n 1)))

(defun org-sql--replace-in-plist (key rep plist)
  "Return PLIST with value of KEY replaced by REP."
  (->> (-partition 2 plist)
       (--map-when (eq (car it) key) (list (car it) rep))
       (-flatten-n 1)))

(defun org-sql--top-section-get-binary-startup (positive negative init top-section)
  "Return startup configuration from TOP-SECTION.
Only keyword nodes with a key of \"STARTUP\" will be considered.
POSITIVE and NEGATIVE are values that represent the states of the
keyword; if it's value is POSITIVE t will be returned, and if it
is NEGATIVE nil will be returned. If no startup keywords are
found, return INIT. If multiple keywords are present, use the
last one."
  (->> top-section
       (--filter (and (org-ml-is-type 'keyword it)
                      (equal "STARTUP" (org-ml-get-property :key it))))
       (--reduce-from (let ((v (org-ml-get-property :value it)))
                        (cond
                         ((equal v positive) t)
                         ((equal v negative) nil)
                         (t acc)))
                      init)))

(defun org-sql--top-section-get-log-into-drawer (top-section log-into-drawer)
  "Return the log-note-headines startup configuration for TOP-SECTION.
If not present, return the current value of LOG-INTO-DRAWER."
  (org-sql--top-section-get-binary-startup
   "logdrawer" "nologdrawer" log-into-drawer top-section))

(defun org-sql--top-section-get-clock-out-notes (top-section clock-out-notes)
  "Return the clock-out-notes startup configuration for TOP-SECTION.
If not present, return the current value of CLOCK-OUT-NOTES."
  (org-sql--top-section-get-binary-startup
   "lognoteclock-out" "nolognoteclock-out" clock-out-notes top-section))

(defun org-sql--to-fstate (file-hash paths-with-attributes log-note-headings
                                     todo-keywords lb-config tree)
  "Return a plist representing the state of an org buffer.
The plist will include:
- `:file-hash': the hash of this org file (given by FILE-HASH)
- `:attributes': the ATTRIBUTES list for the file as returned via
  `file-attributes'
- `:top-section': the org-element TREE representation of this
  org-file's top section before the first headline
- `:headline': a list of org-element TREE headlines in this org
  file
- `:lb-config' the same list as that supplied to
  `org-ml-headline-get-supercontents' (based on LB-CONFIG)
- `:log-note-matcher': a list of log-note-matchers for this org
  file as returned by `org-sql--build-log-note-heading-matchers'
  (which depends on TODO-KEYWORDS and LOG-NOTE-HEADINGS)"
  (let* ((children (org-ml-get-children tree))
         (top-section (-some-> (assq 'section children) (org-ml-get-children))))
    (list :file-hash file-hash
          :paths-with-attributes paths-with-attributes
          :top-section top-section
          :headlines (if top-section (cdr children) children)
          :lb-config (->> lb-config
                          (org-sql--map-plist :log-into-drawer
                            (org-sql--top-section-get-log-into-drawer top-section it))
                          (org-sql--map-plist :clock-out-notes
                            (org-sql--top-section-get-clock-out-notes top-section it)))
          :log-note-matcher (org-sql--build-log-note-heading-matchers
                             log-note-headings todo-keywords))))

(defun org-sql--headline-get-log-into-drawer (log-into-drawer headline)
  "Return the log-into-drawer configuration for HEADLINE.
If not present, return the current value of LOG-INTO-DRAWER."
  (-if-let (v (org-ml-headline-get-node-property "LOG_INTO_DRAWER" headline))
    (cond
     ((equal v "t") t)
     ((equal v "nil") nil)
     (t v))
    log-into-drawer))

(defun org-sql--headline-get-clock-into-drawer (clock-into-drawer headline)
  "Return the clock-into-drawer configuration for HEADLINE.
If not present, return the current value of CLOCK-INTO-DRAWER."
  (-if-let (v (org-ml-headline-get-node-property "CLOCK_INTO_DRAWER" headline))
    (cond
     ((equal v "t") t)
     ((equal v "nil") nil)
     ((s-matches-p "[0-9]+" v) (string-to-number v))
     (t v))
    clock-into-drawer))

(defun org-sql--headline-update-supercontents-config (config headline)
  "Return supercontents CONFIG updated according to HEADLINE."
  (->> config
       (org-sql--map-plist :log-into-drawer
         (org-sql--headline-get-log-into-drawer it headline))
       (org-sql--map-plist :clock-into-drawer
         (org-sql--headline-get-clock-into-drawer it headline))))

(defun org-sql--to-hstate (fstate headline)
  "Return new hstate set from FSTATE and HEADLINE.

An HSTATE represents the current headline being processed and
will include the follwing keys/values:
- `:file-hash' the path to the current file being processed
- `:lb-config' the supercontents config plist
- `:log-note-matcher': a list of log-note-matchers for this org
  file as returned by `org-sql--build-log-note-heading-matchers'
- `:headline' the current headline node."
  (-let (((&plist :file-hash h :lb-config c :log-note-matcher m) fstate))
    (list :file-hash h
          :lb-config (org-sql--headline-update-supercontents-config c headline)
          :log-note-matcher m
          :headline headline)))

(defun org-sql--update-hstate (hstate headline)
  "Return a new HSTATE updated with information from HEADLINE.
Only the :lb-config and :headline keys will be changed."
  (->> (org-sql--replace-in-plist :headline headline hstate)
       (org-sql--map-plist :lb-config
         (org-sql--headline-update-supercontents-config it headline))))

;; (defun org-sql--to-fmeta (disk-path db-path file-hash)
;;   "Return a plist representing org file status.
;; DISK-PATH is the path to the org file on disk, DB-PATH is the
;; path on disk recorded in the database for this org file, and
;; FILE-HASH is the md5 of this org file."
;;   (list :disk-path disk-path :db-path db-path :file-hash file-hash))

(defun org-sql--to-fmeta (file-path file-hash)
  (list :file-path file-path :file-hash file-hash))

;;; SQL string parsing functions

(defun org-sql--parse-output-to-plist (config cols out)
  "Parse SQL output string OUT to an plist representing the data.
COLS are the column names as symbols used to obtain OUT.
CONFIG is the `org-sql-db-config' list."
  (unless (equal out "")
    (let ((sep (org-sql--case-mode config
                 (mysql "\t")
                 ((postgres sqlite sqlserver) "|"))))
      (->> (s-trim out)
           (s-split "\n")
           (--map (s-split sep it))
           (--map (-interleave cols it))))))

;;; MQL -> SQL string formatting functions

;; formatting function tree

(defun org-sql--compile-mql-format-function (config type)
  "Return SQL value formatting function.
The returned function will depend on the MODE and TYPE."
  (cl-flet
      ((quote-string
        (s)
        (format "'%s'" s))
       (escape-string
        (newline single-quote s)
        (->> (s-replace-regexp "'" single-quote s)
             (s-replace-regexp "\n" newline))))
    ;; TODO this could be way more elegant (build the lambda with forms)
    (let* ((esc-newline
            (org-sql--case-mode config
              (mysql "\\\\n")
              (postgres "'||chr(10)||'")
              (sqlite "'||char(10)||'")
              ;; TODO not sure if this also needs Char(13) in front of Char(10)
              ;; for the carriage return (alas...newline war)
              (sqlserver "+Char(10)+")))
           (esc-single-quote
            (org-sql--case-mode config
              (mysql "\\\\'")
              ((postgres sqlite sqlserver) "''")))
           (formatter
            (org-sql--case-type type
              (boolean
               (org-sql--case-mode config
                 ((mysql postgres) (lambda (b) (if (= b 1) "TRUE" "FALSE")))
                 ((sqlite sqlserver) (lambda (b) (if (= b 1) "1" "0")))))
              ;; TODO refactor this nonsense...
              (char
               (lambda (s)
                 (quote-string (escape-string esc-newline esc-single-quote s))))
              (enum (lambda (e) (quote-string (symbol-name e))))
              (integer #'number-to-string)
              (text
               (lambda (s)
                 (quote-string (escape-string esc-newline esc-single-quote s))))
              (varchar
               (lambda (s)
                 (quote-string (escape-string esc-newline esc-single-quote s)))))))
      (lambda (s) (if s (funcall formatter s) "NULL")))))

(defun org-sql--compile-mql-schema-formatter-alist (config mql-tables)
  "Return an alist of formatting functions for MQL-TABLES.
MODE is the SQL mode. The alist will mirror MSL schema except that the
car for each column will be a formatting function."
  (cl-flet
      ((get-type-function
        (mql-column)
        (-let* (((name . (&plist :type)) mql-column))
          (cons name (org-sql--compile-mql-format-function config type)))))
    (-let* (((tbl-name . (&alist 'columns)) mql-tables))
      (cons tbl-name (-map #'get-type-function columns)))))

;; helper functions

(defun org-sql--format-mql-plist (formatter-alist sep plist)
  "Format a PLIST to a SQL-compliant string.
FORMATTER-ALIST is an alist of formatting functions matching the keys
in PLIST (whose keys in turn should match columns in the schema).
The keys and values will be formatted like \"key=val\" and
separated by SEP."
  (let ((keys (->> (-slice plist 0 nil 2)
                   (-map #'org-sql--format-mql-column-name)))
        (vals (->> (-partition 2 plist)
                   (--map (funcall (alist-get (car it) formatter-alist) (cadr it))))))
    (-some->> (--zip-with (format "%s=%s" it other) keys vals)
      (s-join sep))))

(defun org-sql--format-mql-column-name (column-name)
  "Return SQL string representation of COLUMN-NAME."
  (if (not (keywordp column-name)) (error "Not a keyword: %s" column-name)
    (s-chop-prefix ":" (symbol-name column-name))))

(defun org-sql--format-mql-table-name (config tbl-name)
  "Return TBL-NAME as a formatted string according to CONFIG."
  (org-sql--case-mode config
    ;; these are straightforward since they don't use namespaces
    ((mysql sqlite)
     (symbol-name tbl-name))
    ;; these require a custom namespace (aka schema) prefix if available
    ((postgres sqlserver)
     (-let (((&plist :schema) (cdr config)))
       (if (not schema) (symbol-name tbl-name)
         (format "%s.%s" schema tbl-name))))))

(defun org-sql--format-mql-enum-name (config enum-name)
  "Return ENUM-NAME as a formatted string according to CONFIG."
  ;; ASSUME only modes that support ENUM will call this
  (-let (((&plist :schema) (cdr config)))
    (if (not schema) enum-name (format "%s.%s" schema enum-name))))

;; create table

(defun org-sql--format-mql-schema-enum-types (config mql-tables)
  "Return a series of CREATE TYPE statements for MQL-SCHEMA.
The SQL statements will create all enum types found in
MQL-TABLES. CONFIG is the `org-sql-db-config'."
  ;; ASSUME only modes that support ENUM will call this
  (-let* ((enum-name (org-sql--format-mql-enum-name config "enum_%s_%s"))
          (fmt (format "CREATE TYPE %s AS ENUM (%%s);" enum-name)))
    (cl-labels
        ((format-column
          (tbl-name mql-column)
          (-let* (((column-name . (&plist :type :allowed)) mql-column)
                  (column-name* (org-sql--format-mql-column-name column-name)))
            (when (and (eq type 'enum) allowed)
              (->> (--map (format "'%s'" it) allowed)
                   (s-join ",")
                   (format fmt tbl-name column-name*)))))
         (format-table
          (mql-table)
          (-let (((table-name . (&alist 'columns)) mql-table))
            (-non-nil (--map (format-column table-name it) columns)))))
      (-mapcat #'format-table mql-tables))))

(defun org-sql--format-mql-schema-column-constraints (mql-column-constraints)
  "Return formatted column constraints for MQL-COLUMN-CONSTRAINTS."
  (cl-flet
      ((format-constraint
        (constraint)
        (pcase constraint
          ('notnull "NOT NULL")
          ('unique "UNIQUE")
          ;; TODO add CHECK?
          ;; TODO add PRIMARY KEY?
          (e (error "Unknown constraint %s" e)))))
    (s-join " " (-map #'format-constraint mql-column-constraints))))

(defun org-sql--format-mql-schema-type (config tbl-name mql-column)
  "Return SQL string for the type of MQL-COLUMN.
CONFIG is the `org-sql-db-config' list and TBL-NAME is the name
of the table."
  (-let* (((column-name . (&plist :type)) mql-column)
          (column-name* (org-sql--format-mql-column-name column-name)))
    ;; TODO use ntext for sql server instead of text?
    (org-sql--case-type type
      (boolean
       (org-sql--case-mode config
         ((mysql postgres) "BOOLEAN")
         (sqlite "INTEGER")
         (sqlserver "BIT")))
      (char
       (org-sql--case-mode config
         ((mysql sqlserver)
          (-let (((&plist :length) (cdr mql-column)))
            (if length (format "CHAR(%s)" length) "CHAR")))
         ;; postgres should use TEXT here because (according to the docs) "there
         ;; is no performance difference among char/varchar/text except for the
         ;; length-checking"
         ((postgres sqlite) "TEXT")))
      (enum
       (org-sql--case-mode config
         (mysql (->> (plist-get (cdr mql-column) :allowed)
                     (--map (format "'%s'" it))
                     (s-join ",")
                     (format "ENUM(%s)")))
         (postgres (->> (format "enum_%s_%s" tbl-name column-name*)
                        (org-sql--format-mql-enum-name config)))
         (sqlite "TEXT")
         (sqlserver (-if-let (length (plist-get (cdr mql-column) :length))
                        (format "VARCHAR(%s)" length)
                      "TEXT"))))
      (integer
       "INTEGER")
      (text
       "TEXT")
      (varchar
       (org-sql--case-mode config
         ((mysql sqlserver)
          (-let (((&plist :length) (cdr mql-column)))
            (if length (format "VARCHAR(%s)" length) "VARCHAR")))
         ;; see above why postgres uses TEXT here
         ((postgres sqlite) "TEXT"))))))

(defun org-sql--format-mql-schema-columns (config tbl-name mql-columns)
  "Return SQL string for MQL-COLUMNS.
CONFIG is the `org-sql-db-config' list and TBL-NAME is the name
of the table."
  (cl-flet
      ((format-column
        (mql-column)
        (-let* (((name . (&plist :constraints)) mql-column)
                (name* (org-sql--format-mql-column-name name))
                (type* (org-sql--format-mql-schema-type config tbl-name mql-column))
                (column-str (format "%s %s" name* type*)))
          (if (not constraints) column-str
            (->> (org-sql--format-mql-schema-column-constraints constraints)
                 (format "%s %s" column-str))))))
    (-map #'format-column mql-columns)))

(defun org-sql--format-mql-schema-table-constraints (config mql-tbl-constraints defer)
  "Return SQL string for MQL-TBL-CONSTRAINTS.
If DEFER is t, add 'INITIALLY DEFERRED' to the end of each
foreign key constraint. CONFIG is the `org-sql-db-config' list."
  (cl-labels
      ((format-primary
        (keyvals)
        (-let* (((&plist :keys) keyvals))
          (->> (-map #'org-sql--format-mql-column-name keys)
               (s-join ",")
               (format "PRIMARY KEY (%s)"))))
       (format-foreign
        (keyvals)
        ;; TODO shouldn't need 'on update' anymore
        (-let* (((&plist :ref :keys :parent-keys :on_delete) keyvals)
                (ref* (org-sql--format-mql-table-name config ref))
                (keys* (->> keys (-map #'org-sql--format-mql-column-name) (s-join ",")))
                (parent-keys* (->> parent-keys
                                   (-map #'org-sql--format-mql-column-name)
                                   (s-join ",")))
                (foreign-str (format "FOREIGN KEY (%s) REFERENCES %s (%s)"
                                     keys* ref* parent-keys*))
                (on-delete* (-some--> on_delete
                              (cl-case it
                                (no-action "NO ACTION")
                                (cascade "CASCADE"))
                              (format "ON DELETE %s" it)))
                (deferrable (when defer "DEFERRABLE INITIALLY DEFERRED")))
          (->> (list foreign-str on-delete* deferrable)
               (-non-nil)
               (s-join " "))))
       (format-constraint
        (mql-constraint)
        (pcase mql-constraint
          (`(primary . ,keyvals) (format-primary keyvals))
          (`(foreign . ,keyvals) (format-foreign keyvals)))))
    (-map #'format-constraint mql-tbl-constraints)))

(defun org-sql--format-mql-schema-table (config mql-table)
  "Return CREATE TABLE (...) SQL string for MQL-TABLE.
CONFIG is the `org-sql-db-config' list."
  (-let* (((tbl-name . (&alist 'columns 'constraints)) mql-table)
          (tbl-name* (org-sql--format-mql-table-name config tbl-name))
          (defer (org-sql--case-mode config
                   ((mysql sqlserver) nil)
                   ((postgres sqlite) t)))
          (fmt (org-sql--case-mode config
                 ((mysql postgres sqlite) "CREATE TABLE IF NOT EXISTS %s (%s);")
                 (sqlserver
                  (org-sql--with-config-keys (:database) config
                    (format "IF NOT EXISTS (SELECT * FROM sys.tables where name = '%%1$s') CREATE TABLE %%1$s (%%2$s);" database))))))
    (->> (org-sql--format-mql-schema-table-constraints config constraints defer)
         (append (org-sql--format-mql-schema-columns config tbl-name columns))
         (s-join ",")
         (format fmt tbl-name*))))

(defun org-sql--format-mql-schema (config mql-tables)
  "Return schema SQL string for MQL-TABLES.
CONFIG is the `org-sql-db-config' list."
  (let ((create-tables (->> mql-tables
                            (--map (org-sql--format-mql-schema-table config it))
                            (s-join ""))))
    (org-sql--case-mode config
      (postgres
       (let ((create-types (->> (org-sql--format-mql-schema-enum-types config mql-tables)
                                (s-join ""))))
         (concat create-types create-tables)))
      ((mysql sqlite sqlserver)
       create-tables))))

;; insert

(defun org-sql--format-mql-insert (config formatter-alist mql-insert)
  "Return SQL string for MQL-INSERT.
FORMATTER-ALIST is an alist of functions given by
`org-sql--compile-mql-format-function'. CONFIG is the
`org-sql-db-config' list."
  (-let* (((tbl-name . keyvals) mql-insert)
          (tbl-name* (org-sql--format-mql-table-name config tbl-name))
          (formatter-list (alist-get tbl-name formatter-alist))
          (columns (->> (-slice keyvals 0 nil 2)
                        (-map #'org-sql--format-mql-column-name)
                        (s-join ",")))
          (values (->> (-partition 2 keyvals)
                       (--map (funcall (alist-get (car it) formatter-list) (cadr it)))
                       (s-join ","))))
    (format "INSERT INTO %s (%s) VALUES (%s);" tbl-name* columns values)))

;; update

;; (defun org-sql--format-mql-update (config formatter-alist mql-update)
;;   "Return SQL string for MQL-UPDATE.
;; FORMATTER-ALIST is an alist of functions given by
;; `org-sql--compile-mql-format-function'. CONFIG is the
;; `org-sql-db-config' list."
;;   (-let* (((tbl-name . (&alist 'set 'where)) mql-update)
;;           (tbl-name* (org-sql--format-mql-table-name config tbl-name))
;;           (formatter-list (alist-get tbl-name formatter-alist))
;;           (set* (org-sql--format-mql-plist formatter-list "," set))
;;           (where* (org-sql--format-mql-plist formatter-list " and " where)))
;;     (format "UPDATE %s SET %s WHERE %s;" tbl-name* set* where*)))

;; delete

(defun org-sql--format-mql-delete (config formatter-alist mql-delete)
  "Return SQL string for MQL-DELETE.
FORMATTER-ALIST is an alist of functions given by
`org-sql--compile-mql-format-function'. CONFIG is the
`org-sql-db-config' list."
  (-let* (((tbl-name . (&alist 'where)) mql-delete)
          (tbl-name* (org-sql--format-mql-table-name config tbl-name))
          (formatter-list (alist-get tbl-name formatter-alist)))
    (if (not where) (format "DELETE FROM %s;" tbl-name*)
      (->> (org-sql--format-mql-plist formatter-list " and " where)
           (format "DELETE FROM %s WHERE %s;" tbl-name*)))))

;; select

(defun org-sql--format-mql-select (config formatter-alist mql-select)
  "Return SQL string for MQL-SELECT.
FORMATTER-ALIST is an alist of functions given by
`org-sql--compile-mql-format-function'. CONFIG is the
`org-sql-db-config' list."
  (-let* (((tbl-name . (&alist 'columns 'where)) mql-select)
          (tbl-name* (org-sql--format-mql-table-name config tbl-name))
          (formatter-list (alist-get tbl-name formatter-alist))
          (columns* (or (-some->> (-map #'org-sql--format-mql-column-name columns)
                          (s-join ","))
                        "*")))
    (if (not where) (format "SELECT %s FROM %s;" columns* tbl-name*)
      (->> (org-sql--format-mql-plist formatter-list " AND " where)
           (format "SELECT %s FROM %s WHERE %s;" columns* tbl-name*)))))

;;; SQL string -> SQL string formatting functions

(defun org-sql--format-sql-transaction (config sql-statements)
  "Return SQL string for a transaction.
SQL-STATEMENTS is a list of SQL statements to be included in the

transaction. MODE is the SQL mode."
  (-let ((bare-transaction (-some->> sql-statements
                             (s-join "")
                             (format "BEGIN;%sCOMMIT;"))))
    ;; TODO might want to add performance options here
    (when bare-transaction
      (org-sql--case-mode config
        (sqlite (concat "PRAGMA foreign_keys = ON;" bare-transaction))
        ;; TODO pretty sure that sql server actually needs the full "BEGIN
        ;; TRANSACTION"
        ((mysql postgres sqlserver) bare-transaction)))))

;;; org-element/org-ml wrapper functions

;; TODO these are all functions that may be included in org-ml in the future
        
(defun org-sql--headline-get-path (headline)
  "Return the path for HEADLINE node.
Path will be a list of offsets for the parent headline and its
parents (with HEADLINE on the right end of the list)."
  (cl-labels
      ((get-path
        (acc node)
        (if (or (null node) (eq 'org-data (car node))) acc
          (get-path (cons (org-ml-get-property :begin node) acc)
                    (org-ml-get-property :parent node)))))
    (get-path nil headline)))
        
(defun org-sql--headline-get-archive-itags (headline)
  "Return archive itags from HEADLINE or nil if none."
  (unless org-sql-exclude-inherited-tags
    (-some-> (org-ml-headline-get-node-property "ARCHIVE_ITAGS" headline)
      (split-string))))

(defun org-sql--headline-get-tags (headline)
  "Return list of tags from HEADLINE."
  (->> (org-ml-get-property :tags headline)
       (-map #'substring-no-properties)))

(defun org-sql--headline-get-contents (headline)
  "Return the contents of HEADLINE.
This includes everything in the headline's section element that
is not the planning, logbook drawer, or property drawer."
  (-some->> (org-ml-headline-get-section headline)
    ;; TODO need a function in org-ml that returns non-meta
    ;; TODO this only works when `org-log-into-drawer' is defined
    (--remove (org-ml-is-any-type '(planning property-drawer) it))
    (--remove (and (org-ml-is-type 'drawer it)
                   (equal (org-element-property :drawer-name it)
                          org-log-into-drawer)))))

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

(defun org-sql--item-get-contents (item)
  "Return the children of ITEM that are not items."
  (->> (org-ml-get-children item)
       (--take-while (not (org-ml-is-type 'plain-list it)))))

(defun org-sql--split-item (item)
  "Split the contents of ITEM by the first line break."
  (-let (((first . rest) (org-sql--item-get-contents item)))
    (when first
      (if (not (org-ml-is-type 'paragraph first)) (cons nil rest)
        (-let (((p0 . p1) (org-sql--split-paragraph first)))
          (if (not p0) `(,p1 . ,rest) `(,p0 . (,p1 . ,rest))))))))

;; org-element tree -> logbook entry (see `org-sql--to-entry')

(defun org-sql--build-log-note-regexp-alist (todo-keywords)
  "Return a list of regexps that match placeholders in `org-log-note-headings'.
Each member of list will be like (PLACEHOLDER . REGEXP).
TODO-KEYWORDS is the list of valid todo state keywords for the buffer, and will
be used when evaluating the regexp for the \"%S\" and \"%s\" matchers."
  (cl-flet
      ((format-capture
        (regexp)
        (->> (s-replace-all '(("\\(" . "") ("\\)" . "")) regexp)
             (format "\\(%s\\)"))))
    (let* ((ts-or-todo-regexp (->> (-map #'regexp-quote todo-keywords)
                                   (cons org-ts-regexp-inactive)
                                   (s-join "\\|")
                                   (format-capture)
                                   (format "\"%s\"")))
           (ts-regexp (format-capture org-ts-regexp))
           (ts-ia-regexp (format-capture org-ts-regexp-inactive))
           (keys (-map #'cdr org-sql--log-note-keys)))
      ;; TODO the user/user-full variables have nothing stopping them from
      ;; constraining spaces, in which case this will fail
      (->> (list "\\(.*\\)"
                 "\\(.*\\)"
                 ts-ia-regexp
                 ts-regexp
                 ts-ia-regexp
                 ts-regexp
                 ts-or-todo-regexp
                 ts-or-todo-regexp)
           (--map (format "[[:space:]]*%s[[:space:]]*" it))
           (-zip-pair keys)))))

(defun org-sql--build-log-note-heading-matchers (log-note-headings todo-keywords)
  "Return a list of matchers for LOG-NOTE-HEADINGS.

LOG-NOTE-HEADINGS is an alist like
`org-log-note-headines' (identical if the user has not changed
it). TODO-KEYWORDS is a list of todo state keywords for the current buffer.

Return a list like (TYPE REGEXP (KEYS ...)) where TYPE is the type of the note,
REGEXP is a regular expression matching the header of the note, and KEYS is an
ordered list of keywords from `org-sql--log-note-keys' that correspond to each
capture in REGEXP."
  (cl-labels
      ((reverse-lookup
        (value alist)
        (car (--find (equal (cdr it) value) alist)))
       ;; TODO this will fail if the pattern is in the front; a more direct way
       ;; would be to match the pattern directly; see `org-replace-escapes' for
       ;; the regexp that is used when replacing
       (unpad-headings
        (heading)
        (org-replace-escapes heading org-sql--log-note-replacements))
       (replace-escapes
        (heading replace-alist)
        (-> (s-replace-regexp "\s+" " " heading)
            (org-replace-escapes replace-alist)))
       (match-keys
        (heading)
        (->> (s-match-strings-all "%[[:alpha:]]" heading)
             (-flatten-n 1)
             (--map (reverse-lookup it org-sql--log-note-keys)))))
    (let* ((log-note-headings* (--remove (equal (car it) "") log-note-headings))
           (regexp-alist (org-sql--build-log-note-regexp-alist todo-keywords))
           (types (-map #'car log-note-headings*))
           (unpadded (--map (unpad-headings (cdr it)) log-note-headings*))
           (regexps (--map (replace-escapes it regexp-alist) unpadded))
           (keys (-map #'match-keys unpadded)))
      (-zip-lists types regexps keys))))

(defun org-sql--match-item-header (hstate header-text)
  "Return a plist with the matched captures for HEADER-TEXT.

HSTATE is a list given by `org-sql--to-hstate'.

The returned list will be a list like (TYPE PLIST) where TYPE is
the matched type of the note based on HEADER-TEXT and PLIST is a
list of captures corresponding to `org-sql--log-note-keys'."
  (-let (((&plist :log-note-matcher) hstate))
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
      (or (-reduce-from #'match-header nil log-note-matcher) '(none)))))

(defmacro org-sql--to-entry (type &rest plist)
  "Return a list representing a logbook entry.
TYPE is the type of the entry and PLIST is data for the entry
whose keys are a subset of `org-sql--entry-keys'."
  (declare (indent 1))
  (let ((input-keys (-slice plist 0 nil 2)))
    (-some->> (-difference input-keys org-sql--entry-keys)
      (--map (format "%S" it))
      (s-join ", ")
      (error "Keys not valid for entry: %s"))
    `(list ,type ,@plist)))

(defun org-sql--item-to-entry (hstate item)
  "Return entry list from ITEM.
See `org-sql--to-entry' for the meaning of the returned list.
HSTATE is a list given by `org-sql--to-hstate'."
  (-let* (((&plist :file-hash :headline) hstate)
          (headline-offset (org-ml-get-property :begin headline))
          ((header-node . rest) (org-sql--split-item item))
          (header-offset (org-ml-get-property :begin header-node))
          (header-text (org-ml-to-trimmed-string header-node))
          (note-text (-some->> (-map #'org-ml-to-string rest)
                       (s-join "")
                       (s-trim)))
          ((type . (&plist :user
                           :user-full
                           :ts
                           :ts-active
                           :short-ts
                           :short-ts-active
                           :old-state
                           :new-state))
           (org-sql--match-item-header hstate header-text)))
    (cl-flet
        ((get-timestamp-node
          (match-bounds)
          (when match-bounds
            (let ((ts-offset (+ header-offset (car match-bounds))))
              (->> (org-ml-get-children header-node)
                   (--find (and (org-ml-is-type 'timestamp it)
                                ;; TODO make this function public
                                (org-ml--property-is-eq :begin ts-offset it)))))))
         (get-substring
          (match-bounds)
          (when match-bounds
            (-let (((begin . end) match-bounds))
              (substring header-text begin end)))))
      (let ((old-ts (get-timestamp-node old-state))
            (new-ts (get-timestamp-node new-state)))
        (org-sql--to-entry type
          :file-hash file-hash
          :entry-offset (org-ml-get-property :begin item)
          :headline-offset headline-offset
          :header-text header-text
          :note-text note-text
          :user (get-substring user)
          :user-full (get-substring user-full)
          :ts (get-timestamp-node ts)
          :ts-active (get-timestamp-node ts-active)
          :short-ts (get-timestamp-node short-ts)
          :short-ts-active (get-timestamp-node short-ts-active)
          :old-ts old-ts
          :new-ts new-ts
          :old-state (unless old-ts (get-substring old-state))
          :new-state (unless new-ts (get-substring new-state)))))))

(defun org-sql--clocks-append-notes (hstate clocks)
  "Return list of cells like (CLOCK . NOTE-TEXT).
CLOCKS is a list of clocks which may contain items (the clock
notes). If a clock has a note after it and HSTATE has the
appropriate configuration, said clock will be returned as (CLOCK
. NOTE-TEXT) where the item will be converted to a string and set
to NOTE-TEXT; otherwise just as (CLOCK)."
  (-let (((&plist :lb-config (&plist :clock-out-notes)) hstate))
    (if (not clock-out-notes) (-map #'list clocks)
      (->> clocks
           (--reduce-from
            (if (org-ml-is-type 'item it)
                (let ((note-text (->> (org-ml-get-children it)
                                      (-map #'org-ml-to-string)
                                      (s-join "")
                                      (s-trim))))
                  (cons (cons (car (car acc)) note-text) (cdr acc)))
              (cons (list it) acc))
            nil)
           (reverse)))))

;; org-element tree -> MQL inserts (see `org-sql--mql-insert')

(defun org-sql--add-mql-insert-headline-logbook-item (acc entry)
  "Add MQL-insert for item ENTRY to ACC."
  (-let (((entry-type . (&plist :entry-offset
                                :header-text
                                :note-text
                                :headline-offset
                                :file-hash
                                :ts))
          entry))
    (org-sql--add-mql-insert acc logbook_entries
      :file_hash file-hash
      :headline_offset headline-offset
      :entry_offset entry-offset
      :entry_type (symbol-name entry-type)
      :time_logged (-some->> ts
                     (org-ml-timestamp-get-start-time)
                     (org-ml-time-to-unixtime))
      :header header-text
      :note note-text)))

(defun org-sql--add-mql-insert-state-change (acc entry)
  "Add MQL-insert for state change ENTRY to ACC."
  (-let (((&plist :entry-offset :file-hash :old-state :new-state) (cdr entry)))
    (--> (org-sql--add-mql-insert-headline-logbook-item acc entry)
         (org-sql--add-mql-insert it state_changes
           :file_hash file-hash
           :entry_offset entry-offset
           :state_old old-state
           :state_new new-state))))

(defun org-sql--add-mql-insert-planning-change (acc hstate entry)
  "Add MQL-insert for planning change ENTRY to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let (((&plist :entry-offset :file-hash :headline-offset :old-ts) (cdr entry)))
    (--> (org-sql--add-mql-insert-headline-logbook-item acc entry)
         (org-sql--add-mql-insert-timestamp it hstate old-ts)
         (org-sql--add-mql-insert it planning_changes
           :file_hash file-hash
           :entry_offset entry-offset
           :timestamp_offset (org-ml-get-property :begin old-ts)))))

(defun org-sql--add-mql-insert-headline-logbook-items (acc hstate logbook)
  "Add MQL-inserts for LOGBOOK to ACC.
LOGBOOK is the logbook value of the supercontents list returned
by `org-ml-headline-get-supercontents'. HSTATE is a plist as
returned by `org-sql--to-hstate'."
  ;; TODO what about unknown stuff?
  (-let (((&plist :headline :file-hash) hstate))
    (cl-flet
        ((add-entry
          (acc entry)
          (let ((entry-type (car entry)))
            (cond
             ((memq entry-type org-sql-excluded-logbook-types)
              acc)
             ((memq entry-type '(redeadline deldeadline reschedule delschedule))
                 ;; TODO this is inconsistent and it bugs me
              (org-sql--add-mql-insert-planning-change acc hstate entry))
             ((eq entry-type 'state)
              (org-sql--add-mql-insert-state-change acc entry))
             (t
              (org-sql--add-mql-insert-headline-logbook-item acc entry))))))
      (->> (org-ml-logbook-get-items logbook)
           (--map (org-sql--item-to-entry hstate it))
           (-reduce-from #'add-entry acc)))))

(defun org-sql--add-mql-insert-clock (acc hstate clock note-text)
  "Add MQL-insert for CLOCK to ACC.
NOTE-TEXT is either a string or nil representing the clock-note.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let (((&plist :headline :file-hash) hstate)
         (value (org-ml-get-property :value clock)))
    (org-sql--add-mql-insert acc clocks
      :file_hash file-hash
      :headline_offset (org-ml-get-property :begin headline)
      :clock_offset (org-ml-get-property :begin clock)
      :time_start (-some-> value
                    (org-ml-timestamp-get-start-time)
                    (org-ml-time-to-unixtime))
      :time_end (-some-> value
                  (org-ml-timestamp-get-end-time)
                  (org-ml-time-to-unixtime))
      :clock_note (unless org-sql-exclude-clock-notes note-text))))

(defun org-sql--add-mql-insert-headline-logbook-clocks (acc hstate logbook)
  "Add MQL-inserts for LOGBOOK to ACC.
LOGBOOK is the logbook value of the supercontents list returned
by `org-ml-headline-get-supercontents'. HSTATE is a plist as
returned by `org-sql--to-hstate'."
  (->> (org-ml-logbook-get-clocks logbook)
       (org-sql--clocks-append-notes hstate)
       (--reduce-from
        (org-sql--add-mql-insert-clock acc hstate (car it) (cdr it))
        acc)))

(defun org-sql--add-mql-insert-headline-properties (acc hstate)
  "Add MQL-insert for each property in the current headline to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (if (eq 'all org-sql-excluded-properties) acc
    ;; TODO only do this once
    (-let* ((ignore-list (append org-sql--ignored-properties-default
                                 org-sql-excluded-properties))
            ((&plist :headline :file-hash) hstate)
            (headline-offset (org-ml-get-property :begin headline)))
      (cl-flet
          ((is-ignored
            (node-property)
            (member (org-ml-get-property :key node-property) ignore-list))
           (add-property
            (acc np)
            (let ((property-offset (org-ml-get-property :begin np)))
              (--> (org-sql--add-mql-insert acc properties
                     :file_hash file-hash
                     :property_offset property-offset
                     :key_text (org-ml-get-property :key np)
                     :val_text (org-ml-get-property :value np))
                   (org-sql--add-mql-insert it headline_properties
                     :file_hash file-hash
                     :headline_offset headline-offset
                     :property_offset property-offset)))))
        (->> (org-ml-headline-get-node-properties headline)
             (-remove #'is-ignored)
             (-reduce-from #'add-property acc))))))

(defun org-sql--add-mql-insert-headline-tags (acc hstate)
  "Add MQL-insert for each tag in the current headline to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (if (eq 'all org-sql-excluded-tags) acc
    (-let* (((&plist :headline :file-hash) hstate)
            (offset (org-ml-get-property :begin headline)))
      (cl-flet
          ((add-tag
            (acc tag inherited)
            (org-sql--add-mql-insert acc headline_tags
              :file_hash file-hash
              :headline_offset offset
              :tag tag
              :is_inherited (if inherited 1 0)))
           (filter-ignored
            (tags)
            (-difference tags org-sql-excluded-tags)))
        (let ((tags (filter-ignored (org-sql--headline-get-tags headline)))
              (i-tags (->> (org-sql--headline-get-archive-itags headline)
                           (filter-ignored))))
          (--> acc
               (--reduce-from (add-tag acc it nil) it tags)
               (--reduce-from (add-tag acc it t) it i-tags)))))))

(defun org-sql--add-mql-insert-headline-links (acc hstate contents)
  "Add MQL-insert for each link in the current headline to ACC.
CONTENTS is a list corresponding to that returned by
`org-ml-headline-get-supercontents'. HSTATE is a plist as
returned by `org-sql--to-hstate'."
  (if (eq 'all org-sql-excluded-link-types) acc
    (-let* (((&plist :headline :file-hash) hstate)
            (offset (org-ml-get-property :begin headline))
            (links (->> (--mapcat (org-ml-match '(:any * link) it) contents)
                        (--remove (member (org-ml-get-property :type it)
                                          org-sql-excluded-link-types)))))
      (cl-flet
          ((add-link
            (acc link)
            (org-sql--add-mql-insert acc links
              :file_hash file-hash
              :headline_offset offset
              :link_offset (org-ml-get-property :begin link)
              :link_path (org-ml-get-property :path link)
              :link_text (->> (org-ml-get-children link)
                              (-map #'org-ml-to-string)
                              (s-join ""))
              :link_type (org-ml-get-property :type link))))
        (-reduce-from #'add-link acc links)))))

(defun org-sql--add-mql-insert-timestamp (acc hstate timestamp)
  "Add MQL-insert for TIMESTAMP to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (cl-flet
      ((get-resolution
        (time)
        (when time (if (org-ml-time-is-long time) 1 0))))
    (-let* ((start (org-ml-timestamp-get-start-time timestamp))
            (end (org-ml-timestamp-get-end-time timestamp))
            ((&plist :headline :file-hash) hstate)
            (headline-offset (org-ml-get-property :begin headline)))
      (org-sql--add-mql-insert acc timestamps
        :file_hash file-hash
        :headline_offset headline-offset
        :timestamp_offset (org-ml-get-property :begin timestamp)
        :is_active (if (org-ml-timestamp-is-active timestamp) 1 0)
        :warning_type (org-ml-get-property :warning-type timestamp)
        :warning_value (org-ml-get-property :warning-value timestamp)
        :warning_unit (org-ml-get-property :warning-unit timestamp)
        :repeat_type (org-ml-get-property :repeater-type timestamp)
        :repeat_value (org-ml-get-property :repeater-value timestamp)
        :repeat_unit (org-ml-get-property :repeater-unit timestamp)
        :time_start (org-ml-time-to-unixtime start)
        :start_is_long (get-resolution start)
        :time_end (-some-> end (org-ml-time-to-unixtime))
        :end_is_long (get-resolution end)
        :raw_value (org-ml-get-property :raw-value timestamp)))))

(defun org-sql--add-mql-insert-headline-timestamps (acc hstate contents)
  "Add MQL-insert for each timestamp in the current headline to ACC.
CONTENTS is a list corresponding to that returned by
`org-ml-headline-get-supercontents'. HSTATE is a plist as
returned by `org-sql--to-hstate'."
  (if (eq org-sql-excluded-contents-timestamp-types 'all) acc
    (-if-let (pattern (-some--> org-sql--content-timestamp-types
                        (-difference it org-sql-excluded-contents-timestamp-types)
                        (--map `(:type ',it) it)
                        `(:any * (:and timestamp (:or ,@it)))))
        (-let* (((&plist :headline) hstate)
                (timestamps (--mapcat (org-ml-match pattern it) contents))
                (headline-offset (org-ml-get-property :begin headline)))
          (--reduce-from (org-sql--add-mql-insert-timestamp acc hstate it)
                         acc timestamps))
      acc)))

(defun org-sql--add-mql-insert-headline-planning (acc hstate)
  "Add MQL-insert for each planning timestamp in the current headline to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let (((&plist :headline :file-hash) hstate))
    (-if-let (planning (org-ml-headline-get-planning headline))
        (let ((offset (org-ml-get-property :begin headline)))
          (cl-flet
              ((add-planning-maybe
                (acc type)
                (-if-let (ts (org-ml-get-property type planning))
                    (--> (org-sql--add-mql-insert-timestamp acc hstate ts)
                         (org-sql--add-mql-insert it planning_entries
                           :file_hash file-hash
                           :headline_offset offset
                           :planning_type (->> (symbol-name type)
                                               (s-chop-prefix ":")
                                               (intern))
                           :timestamp_offset (org-ml-get-property :begin ts)))
                  acc)))
            (--> '(:closed :deadline :scheduled)
                 (-difference it org-sql-excluded-headline-planning-types)
                 (-reduce-from #'add-planning-maybe acc it))))
      acc)))

(defun org-sql--add-mql-insert-headline-closures (acc hstate)
  "Add MQL-insert for parent closures from the current headline to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let* (((&plist :headline :file-hash) hstate)
          (offset (org-ml-get-property :begin headline)))
    (cl-flet
        ((add-closure
          (acc parent-offset depth)
          (org-sql--add-mql-insert acc headline_closures
            :file_hash file-hash
            :headline_offset offset
            :parent_offset parent-offset
            :depth depth)))
      (->> (org-sql--headline-get-path headline)
           (reverse)
           (--map-indexed (list it it-index))
           (reverse)
           (--reduce-from (apply #'add-closure acc it) acc)))))

(defun org-sql--add-mql-insert-headline (acc hstate)
  "Add MQL-insert the current headline's metadata to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (cl-flet
      ((effort-to-int
        (s)
        (pcase (-some->> s
                 (string-trim)
                 (s-match "^\\(\\([0-9]+\\)\\|\\([0-9]+\\):\\([0-6][0-9]\\)\\)$")
                 (-drop 2))
          (`(nil ,h ,m) (+ (* 60 (string-to-number h)) (string-to-number m)))
          (`(,m) (string-to-number m)))))
    (-let* (((&plist :file-hash :lb-config :headline) hstate)
            (supercontents (org-ml-headline-get-supercontents lb-config headline))
            (logbook (org-ml-supercontents-get-logbook supercontents))
            (contents (org-ml-supercontents-get-contents supercontents)))
      (--> (org-sql--add-mql-insert acc headlines
             :file_hash file-hash
             :headline_offset (org-ml-get-property :begin headline)
             :headline_text (org-ml-get-property :raw-value headline)
             :keyword (org-ml-get-property :todo-keyword headline)
             :effort (-> (org-ml-headline-get-node-property "Effort" headline)
                         (effort-to-int))
             :priority (-some->> (org-ml-get-property :priority headline)
                         (byte-to-string))
             :is_archived (if (org-ml-get-property :archivedp headline) 1 0)
             :is_commented (if (org-ml-get-property :commentedp headline) 1 0)
             :content (-some->> (-map #'org-ml-to-string contents)
                        (s-join "")))
           (org-sql--add-mql-insert-headline-planning it hstate)
           (org-sql--add-mql-insert-headline-tags it hstate)
           (org-sql--add-mql-insert-headline-properties it hstate)
           (org-sql--add-mql-insert-headline-timestamps it hstate contents)
           (org-sql--add-mql-insert-headline-links it hstate contents)
           (org-sql--add-mql-insert-headline-logbook-clocks it hstate logbook)
           (org-sql--add-mql-insert-headline-logbook-items it hstate logbook)))))

(defun org-sql--add-mql-insert-headlines (acc fstate)
  "Add MQL-insert headlines in FSTATE to ACC.
FSTATE is a list given by `org-sql--to-fstate'."
  (-let (((&plist :headlines) fstate))
    (cl-labels
        ((add-headline
          (acc hstate hl)
          (let ((sub (org-ml-headline-get-subheadlines hl))
                (hstate* (if hstate (org-sql--update-hstate hstate hl)
                           (org-sql--to-hstate fstate hl))))
            (--> (org-sql--add-mql-insert-headline acc hstate*)
                 (org-sql--add-mql-insert-headline-closures it hstate*)
                 (--reduce-from (add-headline acc hstate* it) it sub)))))
      (--reduce-from (add-headline acc nil it) acc headlines))))

(defun org-sql--add-mql-insert-file-tags (acc fstate)
  "Add MQL-insert for each file tag in file to ACC.
FSTATE is a list given by `org-sql--to-fstate'."
  (-let (((&plist :file-hash :top-section) fstate))
    (cl-flet
        ((add-tag
          (acc tag)
          (org-sql--add-mql-insert acc file_tags
            :file_hash file-hash
            :tag tag)))
      (->> (--filter (org-ml-is-type 'keyword it) top-section)
           (--filter (equal (org-ml-get-property :key it) "FILETAGS"))
           (--mapcat (s-split " " (org-ml-get-property :value it)))
           (-uniq)
           (-reduce-from #'add-tag acc)))))

(defun org-sql--add-mql-insert-file-properties (acc fstate)
  "Add MQL-insert for each file property in file to ACC.
FSTATE is a list given by `org-sql--to-fstate'."
  (-let (((&plist :file-hash :top-section) fstate))
    (cl-flet
        ((add-property
          (acc keyword)
          (-let ((offset (org-ml-get-property :begin keyword))
                 ((key value) (--> (org-ml-get-property :value keyword)
                                   (s-split-up-to " " it 1))))
            (--> (org-sql--add-mql-insert acc properties
                   :file_hash file-hash
                   :property_offset offset
                   :key_text key
                   :val_text value)
                 (org-sql--add-mql-insert it file_properties
                   :file_hash file-hash
                   :property_offset offset)))))
      (->> (--filter (org-ml-is-type 'keyword it) top-section)
           (--filter (equal (org-ml-get-property :key it) "PROPERTY"))
           (-reduce-from #'add-property acc)))))

(defun org-sql--add-mql-insert-file-hash (acc fstate)
  "Add MQL-insert for file in FSTATE to ACC.
FSTATE is a list given by `org-sql--to-fstate'."
  (-let (((&plist :file-hash) fstate))
    (org-sql--add-mql-insert acc file_hashes
      :file_hash file-hash
      ;; TODO this is wrong obviously
      :size 0)))

(defun org-sql--add-mql-insert-file-metadata (acc fstate)
  "Add MQL-insert for file in FSTATE to ACC.
FSTATE is a list given by `org-sql--to-fstate'."
  (-let (((&plist :paths-with-attributes :file-hash) fstate))
    (--reduce-from (org-sql--add-mql-insert acc file_metadata
                     :file_hash file-hash
                     :file_path (car it))
                   acc paths-with-attributes)))

(defun org-sql--fstate-to-mql-insert (fstate)
  "Return all MQL-inserts for FSTATE.
FSTATE is a list given by `org-sql--to-fstate'."
  (-> nil
      (org-sql--add-mql-insert-file-hash fstate)
      (org-sql--add-mql-insert-file-metadata fstate)
      (org-sql--add-mql-insert-file-properties fstate)
      (org-sql--add-mql-insert-file-tags fstate)
      (org-sql--add-mql-insert-headlines fstate)
      (reverse)))

(defun org-sql--fmeta-to-mql-delete (file-hash)
  (org-sql--mql-delete file_hashes (:file_hash file-hash)))

(defun org-sql--fmeta-to-mql-path-delete (file-hash file-paths)
  (--map (org-sql--mql-delete file_metadata (:file_path it :file_hash file-hash))
         file-paths))

(defun org-sql--fmeta-to-mql-path-insert (file-hash file-paths)
  (--map (org-sql--mql-insert file_metadata
           :file_path it
           :file_hash file-hash)
         file-paths))

;; fmeta function (see `org-sql--to-fmeta')

(defun org-sql--partition-fmeta (disk-fmeta db-fmeta)
  (let (hash-in-db paths-dont-match files-to-insert paths-to-insert
                   paths-to-delete cur-disk cur-db n)
    (while disk-fmeta
      (setq hash-in-db nil
            paths-dont-match nil
            n (length db-fmeta)
            cur-disk (car disk-fmeta))
      (while (< 0 n)
        (setq cur-db (nth (1- n) db-fmeta))
        (when (equal (plist-get cur-disk :file-hash)
                     (plist-get cur-db :file-hash))
          (when (not (equal (plist-get cur-disk :file-path)
                            (plist-get cur-db :file-path)))
            (setq paths-dont-match t
                  paths-to-delete (cons cur-db paths-to-delete)))
          (setq hash-in-db t
                db-fmeta (-remove-at (1- n) db-fmeta)))
        (setq n (1- n)))
      (if hash-in-db
          (when paths-dont-match
            (setq paths-to-insert (cons cur-disk paths-to-insert)))
        (setq files-to-insert (cons cur-disk files-to-insert)))
      (setq disk-fmeta (cdr disk-fmeta)))
    `((files-to-insert ,@files-to-insert)
      (paths-to-insert ,@paths-to-insert)
      (paths-to-delete ,@paths-to-delete)
      (files-to-delete ,@db-fmeta))))

;; this works for both inserts and renames
(defun org-sql--group-fmetas-by-hash (fmetas)
  (->> (--map (cons (plist-get it :file-hash) (plist-get it :file-path)) fmetas)
       (-group-by #'car)
       (--map (cons (car it) (-map #'cdr (cdr it))))))

(defun org-sql--fmeta-to-hashes (fmetas)
  (--map (plist-get it :file-hash) fmetas))

;; IO helper functions

(defmacro org-sql--on-success (first-form success-form error-form)
  "Run form depending exit code.
FIRST-FORM must return a cons cell like (RC . OUT) where RC is
the return code and OUT is the output string (stdout and/or
stderr). SUCCESS-FORM will be run if RC is 0 and ERROR-FORM will
be run otherwise. In either case, OUT will be bound to the symbol
'it-out'."
  (declare (indent 1))
  (let ((r (make-symbol "rc")))
    `(-let (((,r . it-out) ,first-form))
       (if (= 0 ,r) ,success-form ,error-form))))

(defmacro org-sql--on-success* (first-form success-form)
  "Run form on successful exit code.
This is like `org-sql--on-success' but with '(error it-out)'
supplied for ERROR-FORM. FIRST-FORM and SUCCESS-FORM have the
same meaning."
  (declare (indent 1))
  `(org-sql--on-success ,first-form ,success-form (error it-out)))

;;;
;;; STATEFUL FUNCTIONS
;;;

;;; low-level IO

(defun org-sql--run-command (path args)
  "Execute PATH with ARGS.
Return a cons cell like (RETURNCODE . OUTPUT)."
  (org-sql--run-command* path nil args))

(defun org-sql--run-command* (path file args)
  "Execute PATH with ARGS and FILE routed to stdin.
Return a cons cell like (RETURNCODE . OUTPUT)."
  (with-temp-buffer
    (let ((rc (apply #'call-process path file (current-buffer) nil args)))
      (cons rc (buffer-string)))))

;;; fmeta -> fstate

(defun org-sql--fmeta-get-fstate (file-hash file-paths)
  (let ((paths-with-attributes (--map (cons it (file-attributes it)) file-paths)))
    ;; just pick the first file path to open
    (with-current-buffer (find-file-noselect (car file-paths) t)
      (let ((tree (org-element-parse-buffer))
            (todo-keywords (-map #'substring-no-properties org-todo-keywords-1))
            (lb-config (list :log-into-drawer org-log-into-drawer
                             :clock-into-drawer org-clock-into-drawer
                             :clock-out-notes org-log-note-clock-out)))
        (org-sql--to-fstate file-hash paths-with-attributes
                            org-log-note-headings todo-keywords lb-config
                            tree)))))

;;; reading fmeta from external state

(defun org-sql--disk-get-fmeta ()
  "Get a list of fmeta for org files on disk.
Each fmeta will have it's :db-path set to nil. Only files in
`org-sql-files' will be considered."
  (cl-flet
      ((get-md5
        (fp)
        (org-sql--on-success (org-sql--run-command "md5sum" `(,fp))
          (car (s-split-up-to " " it-out 1))
          (error "Could not get md5")))
       (expand-if-dir
        (fp)
        (if (not (file-directory-p fp)) `(,fp)
          (directory-files fp t "\\`.*\\.org\\(_archive\\)?\\'"))))
    (if (stringp org-sql-files)
        (error "`org-sql-files' must be a list of paths")
      (->> (-mapcat #'expand-if-dir org-sql-files)
           (-map #'expand-file-name)
           (-filter #'file-exists-p)
           (-uniq)
           (--map (org-sql--to-fmeta it (get-md5 it)))))))

(defun org-sql--db-get-fmeta ()
  "Get a list of fmeta for the database.
Each fmeta will have it's :disk-path set to nil."
  (-let* ((columns '(:file_path :file_hash))
          ;; TODO add compile check for this
          (sql-select (org-sql--format-mql-select org-sql-db-config nil `(file_metadata (columns ,@columns)))))
    (org-sql--on-success* (org-sql--send-sql sql-select)
      (->> (s-trim it-out)
           (org-sql--parse-output-to-plist org-sql-db-config columns)
           (--map (-let (((&plist :file_hash h :file_path p) it))
                    (org-sql--to-fmeta p h)))))))

(defun org-sql--get-transactions ()
  "Return SQL string of the update transaction.
This transaction will bring the database to represent the same
state as the orgfiles on disk."
  (-let* ((disk-fmeta (org-sql--disk-get-fmeta))
          (db-fmeta (org-sql--db-get-fmeta))
          (mode (car org-sql-db-config))
          (formatter-alist
           (->> org-sql--mql-tables
                (--map (org-sql--compile-mql-schema-formatter-alist org-sql-db-config it))))
          ((&alist 'files-to-insert
                   'files-to-delete
                   'paths-to-insert
                   'paths-to-delete)
           (org-sql--partition-fmeta disk-fmeta db-fmeta)))
    (cl-flet
        ((file-inserts-to-sql
          (fmeta)
          (->> (org-sql--group-fmetas-by-hash fmeta)
               (--mapcat (->> (org-sql--fmeta-get-fstate (car it) (cdr it))
                              (org-sql--fstate-to-mql-insert)))
               (--map (org-sql--format-mql-insert org-sql-db-config formatter-alist it))))
         (file-deletes-to-sql
          (fmeta)
          (->> (org-sql--fmeta-to-hashes fmeta)
               (--map (->> (org-sql--fmeta-to-mql-delete it)
                           (org-sql--format-mql-delete org-sql-db-config formatter-alist)))))
         (path-inserts-to-sql
          (fmeta)
          (->> (org-sql--group-fmetas-by-hash fmeta)
               (--mapcat (org-sql--fmeta-to-mql-path-insert (car it) (cdr it)))
               (--map (org-sql--format-mql-insert org-sql-db-config formatter-alist it))))
         (path-deletes-to-sql
          (fmeta)
          (->> (org-sql--group-fmetas-by-hash fmeta)
               (--mapcat (org-sql--fmeta-to-mql-path-delete (car it) (cdr it)))
               (--map (org-sql--format-mql-delete org-sql-db-config formatter-alist it)))))
      (->> (append (path-inserts-to-sql paths-to-insert)
                   (path-deletes-to-sql paths-to-delete)
                   (file-deletes-to-sql files-to-delete)
                   (file-inserts-to-sql files-to-insert))
           (org-sql--format-sql-transaction org-sql-db-config)))))

(defun org-sql-dump-update-transactions ()
  "Dump the update transaction to a separate buffer."
  (let ((out (org-sql--get-transactions)))
    (switch-to-buffer "SQL: Org-update-dump")
    (insert (s-replace ";" ";\n" out))))

;;; SQL command wrappers

(defun org-sql--exec-mysql-command-nodb (args)
  "Execute a mysql command with ARGS.
CONFIG-KEYS is the plist component if `org-sql-db-config'.
The connection options for the postgres server. will be handled here."
  (org-sql--with-config-keys (:hostname :port :username :password)
      org-sql-db-config
    (let ((h (-some->> hostname (list "-h")))
          (p (-some->> port (list "-P")))
          (u (-some->> username (list "-u")))
          ;; TODO add a switch for this?
          (protocol-arg '("--protocol=TCP"))
          ;; TODO should this have the -r option?
          (batch-args '("-Ns"))
          (process-environment
           (if (not password) process-environment
             (cons (format "MYSQL_PWD=%s" password) process-environment))))
      (org-sql--run-command org-sql--mysql-exe
                            (append h p u protocol-arg batch-args args)))))

(defun org-sql--exec-mysql-command (args)
  "Execute a mysql command with ARGS.
CONFIG-KEYS is the plist component if `org-sql-db-config'.
The connection options for the postgres server. will be handled here."
  (org-sql--with-config-keys (:database) org-sql-db-config
    (if (not database) (error "No database specified")
      (org-sql--exec-mysql-command-nodb `("-D" ,database ,@args)))))

(defun org-sql--exec-sqlite-command (args)
  "Execute a sqlite command with ARGS.
CONFIG is the plist component if `org-sql-db-config'."
  (org-sql--with-config-keys (:path) org-sql-db-config
    (if (not path) (error "No path specified")
      (org-sql--run-command org-sql--sqlite-exe (cons path args)))))

(defun org-sql--exec-postgres-command-nodb (args)
  "Execute a postgres command with ARGS.
CONFIG-KEYS is a list like `org-sql-db-config'."
  (org-sql--with-config-keys (:hostname :port :username :password)
      org-sql-db-config
    (let ((h (-some->> hostname (list "-h")))
          (p (-some->> port (list "-p")))
          (u (-some->> username (list "-U")))
          (w '("-w"))
          (f (list "-At"))
          (process-environment
           (if (not password) process-environment
             (cons (format "PGPASSWORD=%s" password) process-environment))))
      (org-sql--run-command org-sql--psql-exe (append h p u f w args)))))

(defun org-sql--exec-postgres-command (args)
  "Execute a postgres command with ARGS.
CONFIG-KEYS is the plist component if `org-sql-db-config'. Note this
uses the 'psql' client command in the background."
  (org-sql--with-config-keys (:database) org-sql-db-config
    (if (not database) (error "No database specified")
      (org-sql--exec-postgres-command-nodb `("-d" ,database ,@args)))))

(defun org-sql--exec-sqlserver-command-nodb (args)
  (org-sql--with-config-keys (:hostname :port :username :password)
      org-sql-db-config
    ;; TODO support more than just tcp connections
    (let ((S (list "-S" (format "tcp:%s,%s" hostname port)))
          (U (-some->> username (list "-U")))
          (sep '("-s" "|"))
          (no-headers '("-h" "-1"))
          (process-environment
           (if (not password) process-environment
             (cons (format "SQLCMDPASSWORD=%s" password) process-environment))))
      (org-sql--run-command org-sql--sqlserver-exe (append S U no-headers sep args)))))

(defun org-sql--exec-sqlserver-command (args)
  (org-sql--with-config-keys (:database) org-sql-db-config
    (if (not database) (error "No database specified")
      (org-sql--exec-sqlserver-command-nodb `("-d" ,database ,@args)))))

(defun org-sql--send-sql (sql-cmd)
  "Execute SQL-CMD.
The database connection will be handled transparently."
  (org-sql--case-mode org-sql-db-config
    (mysql
     (org-sql--exec-mysql-command `("-e" ,sql-cmd)))
    (postgres
     (org-sql--exec-postgres-command `("-c" ,sql-cmd)))
    (sqlite
     (org-sql--exec-sqlite-command `(,sql-cmd)))
    (sqlserver
     (let ((cmd (format "set nocount on; %s" sql-cmd)))
       (org-sql--exec-sqlserver-command `("-Q" ,cmd))))))

(defun org-sql--send-sql* (sql-cmd)
  "Execute SQL-CMD as a separate file input.
The database connection will be handled transparently."
  ;; TODO I don't think there are cases where I want to send a nil cmd, so
  ;; nil should be an error
  (if (not sql-cmd) '(0 . "")
    (-let ((tmp-path (->> (round (float-time))
                          (format "%sorg-sql-cmd-%s" (temporary-file-directory)))))
      (f-write sql-cmd 'utf-8 tmp-path)
      (let ((res
             (org-sql--case-mode org-sql-db-config
               (mysql
                (org-sql--send-sql (format "source %s" tmp-path)))
               (postgres
                (org-sql--send-sql (format "\\i %s" tmp-path)))
               (sqlite
                (org-sql--send-sql (format ".read %s" tmp-path)))
               (sqlserver
                ;; I think ":r tmp-path" should work here to make this analogous
                ;; with the others
                (org-sql--exec-sqlserver-command "-i" tmp-path)))))
        (f-delete tmp-path)
        res))))

;;;
;;; Public API
;;;

;; There are three layers which we care about: the database itself, the
;; namespace, and the tables in which the data will live. And these three have
;; three operations we care about: create, drop, and testing for existence. The
;; namespace (aka the "schema") is only defined for postgres and sql server.
;; Furthermore, we can't assume the normal user has permissions to create the
;; database itself for any of the "server" implementations (everything but
;; SQLite), so the create/drop commands aren't defined for these either. The
;; only layer that is defined for all three operations and all database is the
;; table layer.
;;
;; The reason this matters is because there needs to be a way to define
;; "composite" functions like "initialize" and "reset" a database from emacs.
;; Because all supported DBMSs have different layers, these will mean different
;; things and must take these restrictions into account.

;; table layer

(defun org-sql-create-tables ()
  (let ((sql-cmd (org-sql--format-mql-schema org-sql-db-config org-sql--mql-tables)))
    (let ((res (org-sql--send-sql sql-cmd)))
      res)))

(defun org-sql-drop-tables ()
  (org-sql--case-mode org-sql-db-config
    (mysql
     (->> (s-join "," org-sql-table-names)
          (format "DROP TABLE IF EXISTS %s;")
          (format "SET FOREIGN_KEY_CHECKS = 0;%sSET FOREIGN_KEY_CHECKS = 1;")
          (org-sql--send-sql)))
    (postgres
     (org-sql--with-config-keys (:schema) org-sql-db-config
       (->> (if schema (--map (format "%s.%s" schema it) org-sql-table-names)
              org-sql-table-names)
            (s-join ",")
            (format "DROP TABLE IF EXISTS %s CASCADE;")
            (org-sql--send-sql))))
    (sqlite
     (->> (--map (format "DROP TABLE IF EXISTS %s;" it) org-sql-table-names)
          ;; TODO figure out a better way to format the transaction
          (s-join "")
          (format "PRAGMA foreign_keys = OFF;BEGIN;%sCOMMIT;")
          (org-sql--send-sql)))
    (sqlserver
     (error "This hasn't been defined yet; go yell at the developer"))))

(defun org-sql-list-tables ()
  (-let (((sql-cmd parse-fun)
          (org-sql--case-mode org-sql-db-config
            (mysql
             (list "SHOW TABLES;"
                   (lambda (s)
                     (let ((s* (s-trim s)))
                       (unless (equal s* "") (s-lines s*))))))
            (postgres
             (org-sql--with-config-keys (:schema) org-sql-db-config
               (let ((schema* (or schema "public")))
                 (list (format "\\dt %s.*" schema*)
                       (lambda (s)
                         (->> (s-trim s)
                              (s-lines)
                              (--map (s-split "|" it))
                              (--filter (equal schema* (car it)))
                              (--map (cadr it))))))))
            (sqlite
             (list ".tables"
                   (lambda (s)
                     (--mapcat (s-split " " it t) (s-lines s)))))
            (sqlserver
             (org-sql--with-config-keys (:schema) org-sql-db-config
               (let* ((schema* (or schema "dbo")))
                 (list (format "SELECT schema_name(schema_id), name FROM sys.tables;" schema*)
                       (lambda (s)
                         (->> (s-trim s)
                              (s-lines)
                              (--map (s-split "|" it))
                              (--filter (equal schema* (car it)))
                              (--map (cadr it)))))))))))
    (org-sql--on-success* (org-sql--send-sql sql-cmd)
      (funcall parse-fun it-out))))

;; namespace layer

(defun org-sql-create-namespace ()
  "Create the configured database."
  (org-sql--case-mode org-sql-db-config
    ((mysql sqlite)
     (error "Namespace schemas only exist for Postgres and SQL Server"))
    (postgres
     (org-sql--with-config-keys (:schema) org-sql-db-config
       (let ((cmd (format "CREATE SCHEMA IF NOT EXISTS %s;" (or schema "public"))))
         (org-sql--send-sql cmd))))
    (sqlserver
     (org-sql--with-config-keys (:schema) org-sql-db-config
       (when schema
         (let* ((select (format "SELECT * FROM sys.schemas WHERE name = N'%s'" schema))
                (create (format "CREATE SCHEMA %s" schema))
                (cmd (format "IF NOT EXISTS (%s) EXEC('%s');" select create)))
           (org-sql--send-sql cmd)))))))

(defun org-sql-drop-namespace ()
  (org-sql--case-mode org-sql-db-config
    ((mysql sqlite)
     (error "Namespace schemas only exist for Postgres and SQL Server"))
    (postgres
     (org-sql--with-config-keys (:schema) org-sql-db-config
       (let ((cmd (format "DROP SCHEMA IF EXISTS %s CASCADE;" (or schema "public"))))
         (org-sql--send-sql cmd))))
    (sqlserver
     (org-sql--with-config-keys (:schema) org-sql-db-config
       (when schema
         (let* ((select (format "SELECT * FROM sys.schemas WHERE name = N'%s'" schema))
                (drop (format "DROP SCHEMA %s" schema))
                (cmd (format "IF EXISTS (%s) EXEC('%s');" select drop)))
           (org-sql--send-sql cmd)))))))

(defun org-sql-namespace-exists ()
  ;; NOTE: namespace = "schema" for all databases that have "schemas"
  (org-sql--case-mode org-sql-db-config
    ((mysql sqlite)
     (error "Namespace schemas only exist for Postgres and SQL Server"))
    (postgres
     (org-sql--with-config-keys (:database :schema) org-sql-db-config
       (org-sql--on-success* (org-sql--send-sql "\\dn")
         (--> (s-split "\n" it-out)
           (--map (s-split "|" it) it)
           ;; TODO this (or schema "public") thing is silly and should be
           ;; refactored into something civilized (see other instances below)
           (--find (and (equal (car it) (or schema "public"))
                        (equal (cadr it) database))
                   it)
           (and it t)))))
    (sqlserver
     (org-sql--with-config-keys (:database :schema) org-sql-db-config
       (let ((cmd "select name from [org_sql].sys.schemas;"))
         (org-sql--on-success* (org-sql--send-sql cmd)
           (--> (s-split "\n" it-out)
             (--map (s-split "|" it) it)
             ;; TODO get the default schema name and check that
             (--find (and (equal (car it) (or schema "dbo"))
                          (equal (cadr it) database))
                     it)
             (and it t))))))))

;; database layer

(defun org-sql-create-db ()
  (org-sql--case-mode org-sql-db-config
    ((mysql postgres sqlserver)
     (error "Must manually create database using admin privileges"))
    (sqlite
     ;; this is a silly command that should work on all platforms (eg doesn't
     ;; require `touch' to make an empty file)
     (org-sql--exec-sqlite-command '(".schema")))))

(defun org-sql-drop-db ()
  (org-sql--case-mode org-sql-db-config
    ((mysql postgres sqlserver)
     (error "Must manually drop database using admin privileges"))
    (sqlite
     (org-sql--with-config-keys (:path) org-sql-db-config
       (delete-file path)
       ;; return a dummy return-code and stdout message
       '(0 . "")))))

(defun org-sql-db-exists ()
  (org-sql--case-mode org-sql-db-config
    (mysql
     (org-sql--with-config-keys (:database) org-sql-db-config
       (let ((cmd (format "SHOW DATABASES LIKE '%s';" database)))
         (org-sql--on-success* (org-sql--exec-mysql-command-nodb `("-e" ,cmd))
           (equal database (car (s-lines it-out)))))))
    (postgres
     (org-sql--with-config-keys (:database) org-sql-db-config
       (let ((cmd (format "SELECT 1 FROM pg_database WHERE datname='%s';" database)))
         (org-sql--on-success* (org-sql--exec-postgres-command-nodb `("-c" ,cmd))
           (equal "1" (s-trim it-out))))))
    (sqlite
     (org-sql--with-config-keys (:path) org-sql-db-config
       (file-exists-p path)))
    (sqlserver
     (org-sql--with-config-keys (:database) org-sql-db-config
       (error "This is not defined yet; go yell at the developer")))))

;;; composite database functions

(defun org-sql-init-db ()
  (org-sql--case-mode org-sql-db-config
    (mysql
     nil)
    ((postgres sqlserver)
     (org-sql-create-namespace))
    (sqlite
     (org-sql-create-db)))
  (org-sql-create-tables))

(defun org-sql-update-db ()
  (let ((inhibit-message t))
    (org-save-all-org-buffers))
  (org-sql--send-sql* (org-sql--get-transactions)))

(defun org-sql-clear-db ()
  ;; only delete from files as we assume actions here cascade down
  (->> (org-sql--mql-delete file_hashes nil)
       (org-sql--format-mql-delete org-sql-db-config nil)
       (list)
       (org-sql--format-sql-transaction org-sql-db-config)
       (org-sql--send-sql)))

(defun org-sql-reset-db ()
  (org-sql--case-mode org-sql-db-config
    ;; TODO might make sense to provide this as an option for the others (eg
    ;; maybe they don't want to delete an entire schema when dropping the
    ;; tables will do)
    (mysql
     (org-sql-drop-tables))
    ((postgres sqlserver)
     (org-sql-drop-namespace))
    (sqlite
     (org-sql-drop-db)))
  (org-sql-init-db))

;;; interactive functions

(defun org-sql-user-update ()
  "Update the Org SQL database."
  (interactive)
  ;; TODO need to see if schema is correct?
  (message "Updating Org SQL database")
  (let ((out (org-sql-update-db)))
    (when org-sql-debug
      (print "Debug output for org-sql update")
      (print (if (equal out "") "Run Successfully" out))))
  (message "Org SQL update complete"))

(defun org-sql-user-clear-all ()
  "Remove all entries in the database."
  (interactive)
  (if (y-or-n-p "Really clear all? ")
      (progn
        (message "Clearing Org SQL database")
        (let ((out (org-sql-clear-db)))
          (when org-sql-debug
            (print "Debug output for org-sql clear-all")
            (print (if (equal out "") "Run Successfully" out))))
        (message "Org SQL clear completed"))
    (message "Aborted")))

(defun org-sql-user-reset ()
  "Reset the database with default schema."
  (interactive)
  (if (or (not (org-sql-db-exists))
          (y-or-n-p "Really reset database? "))
      (progn
        (org-sql-drop-db)
        (message "Resetting Org SQL database")
        (let ((out (org-sql-init-db)))
          (when org-sql-debug
            (print "Debug output for org-sql user-reset")
            (print (if (equal out "") "Run Successfully" out))))
        (message "Org SQL reset completed"))
    (message "Aborted")))

(provide 'org-sql)
;;; org-sql.el ends here
