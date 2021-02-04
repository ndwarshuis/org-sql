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
   '(:file-hash :note-text :header-text :old-ts :new-ts))
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

(eval-and-compile
  ;; TODO use UUID type where possible for the hash column
  (let ((file_hash-char-length 32)
        ;; ASSUME all filesystems we would ever want to use have a path limit of
        ;; 255 chars (which is almost always true)
        (file_path-varchar-length 255)
        (tag-varchar-length 32)
        (tag-col
         '(:tag :desc "the text value of this tag"
                :type varchar
                :length 32))
        (property-id-col
         '(:property_id :desc "id of this property"
                        :type integer))
        (modifier-allowed-units '(hour day week month year)))
    (cl-flet*
        ((mql-col
          (default-desc fmt name other object notnull)
          (let* ((d (if object (format fmt object) default-desc))
                 (k `(,name :desc ,d ,@other)))
            (if notnull `(,@k :constraints (notnull)) k)))
         (file-hash-col
          (&optional object notnull)
          (mql-col "hash (MD5) of this org-tree"
                   "hash (MD5) of the org-tree with this %s"
                   :file_hash `(:type char :length ,file_hash-char-length)
                   object notnull))
         (headline-id-col
          (&optional object notnull)
          (mql-col "id of this headline"
                   "id of the headline for this %s"
                   :headline_id '(:type integer) object notnull))
         (timestamp-id-col
          (&optional object notnull)
          (mql-col "id of this timestamp"
                   "id of the timestamp for this %s"
                   :timestamp_id '(:type integer) object notnull))
         (entry-id-col
          (&optional object notnull)
          (mql-col "id of this entry"
                   "id of the entry for this %s"
                   :entry_id '(:type integer) object notnull)))
      (defconst org-sql--mql-tables
        `((file_hashes
           (desc . "Each row describes one org file (which may have multiple filepaths)")
           (columns
            ,(file-hash-col)
            (:tree_size :desc "number of characters of the org tree"
                        :type integer
                        :constraints (notnull))
            (:tree_lines :desc "number of lines in the org file"
                         :type integer
                         :constraints (notnull)))
           (constraints
            (primary :keys (:file_hash))))

          (file_metadata
           (desc . "Each row stores filesystem metadata for one tracked org file")
           (columns
            (:file_path :desc "path to org file"
                        :type varchar
                        :length ,file_path-varchar-length)
            ,(file-hash-col "path" t)
            (:file_uid :desc "UID of the file"
                       :type integer
                       :constraints (notnull))
            (:file_gid :desc "GID of the file"
                       :type integer
                       :constraints (notnull))
            (:file_modification_time :desc "time of the file's last modification"
                                     :type integer
                                     :constraints (notnull))
            (:file_attr_change_time :desc "time of the file's last attribute change"
                                    :type integer
                                    :constraints (notnull))
            (:file_modes :desc "permission mode bits for the file"
                         :type varchar
                         :length 10
                         :constraints (notnull)))
           (constraints
            (primary :keys (:file_path))
            (foreign :ref file_hashes
                     :keys (:file_hash)
                     :parent-keys (:file_hash)
                     ;; TODO this 'on_delete' is not lispy because it has a '_'
                     :on-delete cascade)))

          (headlines
           (desc . "Each row stores one headline in a given org file and its metadata")
           (columns
            ,(headline-id-col)
            ,(file-hash-col "headline")
            (:headline_text :desc "raw text of the headline"
                            :type text
                            :constraints (notnull))
            (:keyword :desc "the TODO state keyword"
                      :type text)
            (:effort :desc "the value of the Effort property in minutes"
                     :type integer)
            (:priority :desc "character value of the priority"
                       :type text)
            (:stats_cookie_type :desc "type of the statistics cookie"
                                :type enum
                                :allowed (fraction percent))
            (:stats_cookie_value :desc "value of the statistics cookie"
                                 :type real)
            (:is_archived :desc "true if the headline has an archive tag"
                          :type boolean
                          :constraints (notnull))
            (:is_commented :desc "true if the headline has a comment keyword"
                           :type boolean
                           :constraints (notnull))
            (:content :desc "the headline contents"
                      :type text))
           (constraints
            (primary :keys (:headline_id))
            (foreign :ref file_hashes
                     :keys (:file_hash)
                     :parent-keys (:file_hash)
                     :on-delete cascade)))

          (headline_closures
           (desc . "Each row stores the ancestor and depth of a headline relationship (eg closure table)")
           (columns
            ,(headline-id-col)
            (:parent_id :desc "id of this headline's parent"
                        :type integer)
            (:depth :desc "levels between this headline and the referred parent"
                    :type integer))
           (constraints
            (primary :keys (:headline_id :parent_id))
            (foreign :ref headlines
                     :keys (:headline_id)
                     :parent-keys (:headline_id)
                     :on-delete cascade)
            (foreign :ref headlines
                     :keys (:parent_id)
                     :parent-keys (:headline_id))))

          (timestamps
           (desc . "Each row stores one timestamp")
           (columns
            ,(timestamp-id-col)
            ,(headline-id-col "timestamp" t)
            (:raw_value :desc "text representation of this timestamp"
                        :type text
                        :constraints (notnull))
            (:is_active :desc "true if the timestamp is active"
                        :type boolean
                        :constraints (notnull))
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
            (primary :keys (:timestamp_id))
            (foreign :ref headlines
                     :keys (:headline_id)
                     :parent-keys (:headline_id)
                     :on-delete cascade)))

          (timestamp_warnings
           (desc . "Each row stores specific information for a timestamp warning")
           (columns
            ,(timestamp-id-col "warning")
            (:warning_value :desc "shift of this warning"
                             :type integer)
            (:warning_unit :desc "unit of this warning"
                            :type enum
                            :allowed ,modifier-allowed-units)
            (:warning_type :desc "type of this warning"
                           :type enum
                           :allowed (all first)))
           (constraints
            (primary :keys (:timestamp_id))
            (foreign :ref timestamps
                     :keys (:timestamp_id)
                     :parent-keys (:timestamp_id)
                     :on-delete cascade)))
          
          (timestamp_repeaters
           (desc . "Each row stores specific information for a timestamp repeater")
           (columns
            ,(timestamp-id-col "repeater")
            (:repeater_value :desc "shift of this repeater"
                             :type integer)
            (:repeater_unit :desc "unit of this repeater"
                            :type enum
                            :allowed ,modifier-allowed-units)
            (:repeater_type :desc "type of this repeater"
                           :type enum
                           :allowed (catch-up restart cumulate)))
           (constraints
            (primary :keys (:timestamp_id))
            (foreign :ref timestamps
                     :keys (:timestamp_id)
                     :parent-keys (:timestamp_id)
                     :on-delete cascade)))

          (planning_entries
           (desc . "Each row stores the metadata for headline planning timestamps.")
           (columns
            ,(headline-id-col "planning entry")
            (:planning_type :desc "the type of this planning entry"
                            :type enum
                            :length 9
                            :allowed (closed scheduled deadline))
            ,(timestamp-id-col "planning entry" t))
           (constraints
            (primary :keys (:headline_id :planning_type))
            (foreign :ref timestamps
                     :keys (:timestamp_id)
                     :parent-keys (:timestamp_id)
                     :on-delete cascade)))

          (file_tags
           (desc . "Each row stores one tag at the file level")
           (columns
            ,(file-hash-col "tag")
            ,tag-col)
           (constraints
            (primary :keys (:file_hash :tag))
            (foreign :ref file_hashes
                     :keys (:file_hash)
                     :parent-keys (:file_hash)
                     :on-delete cascade)))

          (headline_tags
           (desc . "Each row stores one tag")
           (columns
            ,(headline-id-col "tag")
            ,tag-col
            (:is_inherited :desc "true if this tag is from the ITAGS property"
                           :type boolean
                           :constraints (notnull)))
           (constraints
            (primary :keys (:headline_id :tag :is_inherited))
            (foreign :ref headlines
                     :keys (:headline_id)
                     :parent-keys (:headline_id)
                     :on-delete cascade)))

          (properties
           (desc . "Each row stores one property")
           (columns
            ,(file-hash-col "property")
            ,property-id-col
            (:key_text :desc "this property's key"
                       :type text
                       :constraints (notnull))
            (:val_text :desc "this property's value"
                       :type text
                       :constraints (notnull)))
           (constraints
            (primary :keys (:property_id))
            (foreign :ref file_hashes
                     :keys (:file_hash)
                     :parent-keys (:file_hash)
                     :on-delete cascade)))

          (headline_properties
           (desc . "Each row stores a property at the headline level")
           (columns
            ,(headline-id-col "property" t)
            ,property-id-col)
           (constraints
            (primary :keys (:property_id))
            (foreign :ref properties
                     :keys (:property_id)
                     :parent-keys (:property_id))
                     ;; :on-delete cascade)
            (foreign :ref headlines
                     :keys (:headline_id)
                     :parent-keys (:headline_id)
                     :on-delete cascade)))
          
          (clocks
           (desc . "Each row stores one clock entry")
           (columns
            (:clock_id :desc "id of this clock"
                       :type integer)
            ,(headline-id-col "clock" t)
            (:time_start :desc "timestamp for the start of this clock"
                         :type integer)
            (:time_end :desc "timestamp for the end of this clock"
                       :type integer)
            (:clock_note :desc "the note entry beneath this clock"
                         :type text))
           (constraints
            (primary :keys (:clock_id))
            (foreign :ref headlines
                     :keys (:headline_id)
                     :parent-keys (:headline_id)
                     :on-delete cascade)))

          (logbook_entries
           (desc . "Each row stores one logbook entry (except for clocks)")
           (columns
            ,(entry-id-col)
            ,(headline-id-col "logbook entry" t)
            (:entry_type :desc "type of this entry (see `org-log-note-headlines')"
                         :type text)
            (:time_logged :desc "timestamp for when this entry was taken"
                          :type integer)
            (:header :desc "the first line of this entry (usually standardized)"
                     :type text)
            (:note :desc "the text of this entry underneath the header"
                   :type text))
           (constraints
            (primary :keys (:entry_id))
            (foreign :ref headlines
                     :keys (:headline_id)
                     :parent-keys (:headline_id)
                     :on-delete cascade)))

          (state_changes
           (desc . "Each row stores additional metadata for a state change logbook entry")
           (columns
            ,(entry-id-col "state change")
            (:state_old :desc "former todo state keyword"
                        :type text
                        :constraints (notnull))
            (:state_new :desc "updated todo state keyword"
                        :type text
                        :constraints (notnull)))
           (constraints
            (primary :keys (:entry_id))
            (foreign :ref logbook_entries
                     :keys (:entry_id)
                     :parent-keys (:entry_id)
                     :on-delete cascade)))

          (planning_changes
           (desc . "Each row stores additional metadata for a planning change logbook entry")
           (columns
            ,(entry-id-col "planning change")
            (:timestamp_id :desc "id of the former timestamp"
                           :type integer
                           :constraints (notnull)))
           (constraints
            (primary :keys (:entry_id))
            (foreign :ref timestamps
                     :keys (:timestamp_id)
                     :parent-keys (:timestamp_id))
                     ;; :on-delete cascade)
            (foreign :ref logbook_entries
                     :keys (:entry_id)
                     :parent-keys (:entry_id)
                     :on-delete cascade)))

          (links
           (desc . "Each row stores one link")
           (columns
            (:link_id :desc "id of this link"
                      :type integer)
            ,(headline-id-col "link" t)
            (:link_path :desc "target of this link (eg url, file path, etc)"
                        :type text
                        :constraints (notnull))
            (:link_text :desc "text of this link"
                        :type text)
            (:link_type :desc "type of this link (eg http, mu4e, file, etc)"
                        :type text
                        :constraints (notnull)))
           (constraints
            (primary :keys (:link_id))
            (foreign :ref headlines
                     :keys (:headline_id)
                     :parent-keys (:headline_id)
                     :on-delete cascade))))
      "Org-SQL database schema represented in internal meta query
    language (MQL, basically a giant list)"))))

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

(defcustom org-sql-async nil
  "When t database updates will be asynchronous.
All admin operations will still be synchronous. Note that this
only spawns a process for the database client command; all
processing the needs to be performed on org files (parsing to
make the INSERT statements) will still be synchronous."
  :type 'boolean
  :group 'org-sql)

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

(defcustom org-sql-exclude-headline-predicate nil
  "A function that is called for each headline.
If it returns t, the current headline is to be excluded. Note
that excluding a headline will also exclude its children."
  :type 'function
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
  (-let (((&alist 'boolean 'char 'enum 'integer 'real 'text 'varchar)
          (--splice (listp (car it))
                    (-let (((keys . form) it))
                      (--map (cons it form) keys))
                    alist-forms)))
    (when (-any? #'null (list boolean char enum real integer text varchar))
      (error "Must provide form for all types"))
    `(cl-case ,type
       (boolean ,@boolean)
       (char ,@char)
       (enum ,@enum)
       (integer ,@integer)
       (real ,@real)
       (text ,@text)
       (varchar ,@varchar)
       (t (error "Invalid type: %s" ,type)))))

(defmacro org-sql--case-mode (config &rest alist-forms)
  (declare (indent 1))
  (-let (((keys &as &alist 'mysql 'pgsql 'sqlserver 'sqlite)
          (--splice (listp (car it))
                    (-let (((keys . form) it))
                      (--map (cons it form) keys))
                    alist-forms)))
    (when (-any? #'null (list mysql pgsql sqlserver sqlite))
      (error "Must provide form for all modes"))
    (-some->> (-difference (-map #'car keys) '(mysql pgsql sqlserver sqlite))
      (-map #'symbol-name)
      (s-join ",")
      (format "Unknown forms: %s")
      (error))
    `(cl-case (car ,config)
       (mysql ,@mysql)
       (pgsql ,@pgsql)
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

(defun org-sql--get-config-key (key config)
  (plist-get (cdr config) key))

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

(defun org-sql--mql-check-columns (tbl-name columns)
  (let ((valid-columns (org-sql--mql-check-get-schema-keys tbl-name)))
    (-some->> (-difference valid-columns columns)
      (error "Keys not given for table %s: %s" tbl-name))
    (-some->> (-difference columns valid-columns)
      (error "Keys not valid for table %s: %s" tbl-name))))

(defun org-sql--mql-check-columns-all (tbl-name plist)
  "Test if keys in PLIST are valid column names for TBL-NAME.
All column keys must be in PLIST."
  (org-sql--mql-check-columns tbl-name (-slice plist 0 nil 2)))
  ;; (declare (indent 2))
  ;; (let ((valid-keys (org-sql--mql-check-get-schema-keys tbl-name))
  ;;       (input-keys (->> (-partition 2 plist)
  ;;                        (-map #'car))))
  ;;   (-some->> (-difference valid-keys input-keys)
  ;;     (error "Keys not given for table %s: %s" tbl-name))
  ;;   (-some->> (-difference input-keys valid-keys)
  ;;     (error "Keys not valid for table %s: %s" tbl-name))))

(defun org-sql--mql-order-columns (tbl-name plist)
  "Order the keys in PLIST according to the columns in TBL-NAME."
  (org-sql--mql-check-columns-all tbl-name plist)
  (let ((order (->> (alist-get tbl-name org-sql--mql-tables)
                    (alist-get 'columns)
                    (--map-indexed (cons (car it) it-index)))))
    (->> (-partition 2 plist)
         (--map (cons (alist-get (car it) order) (cadr it)))
         (--sort (< (car it) (car other)))
         (-map #'cdr))))

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

(defconst org-sql--empty-mql-bulk-insert
  (--map (list (car it)) org-sql--mql-tables)
  "Empty bulk MQL insert to initialize accumulators.")

(defmacro org-sql--add-mql-insert (acc tbl-name &rest plist)
  "Add a new MQL-insert list for TBL-NAME to ACC.
PLIST is a property list of the columns and values to insert."
  (declare (indent 2))
  (let ((ordered-values (org-sql--mql-order-columns tbl-name plist)))
    ;; WARNING this has side effects (but is super fast because of it)
    `(let ((tbl (assq ',tbl-name (plist-get ,acc :inserts))))
       (setcdr tbl (cons (list ,@ordered-values) (cdr tbl)))
       ,acc)))

(defmacro org-sql--mql-bulk-delete (tbl-name columns tuples)
  (org-sql--mql-check-columns tbl-name columns)
  `(append (list ',tbl-name ',columns) ,tuples))

;; (defmacro org-sql--mql-update (tbl-name set where)
;;   "Return an MQL-update list for TBL-NAME.
;; SET is a plist for the updated values of columns and WHERE is a plist of
;; columns that must be equal to the values in the plist in order for the update
;; to be applied."
;;   (declare (indent 1))
;;   (org-sql--mql-check-columns-contains tbl-name set)
;;   (org-sql--mql-check-columns-contains tbl-name where)
;;   `(list ',tbl-name
;;          (list 'set ,@set)
;;          (list 'where ,@where)))

;; (defmacro org-sql--mql-delete (tbl-name where)
;;   "Return an MQL-delete list for TBL-NAME.
;; WHERE is a plist of columns that must be equal to the values in
;; the plist in order for the delete to be applied."
;;   (org-sql--mql-check-columns-contains tbl-name where)
;;   `(list ',tbl-name
;;          (list 'where ,@where)))

;; external state

;; TODO this is hilariously inefficient
(defmacro org-sql--map-plist (key form plist)
  "Return PLIST modified with FORM applied to KEY's value."
  (declare (indent 1))
  `(let ((it (plist-get ,plist ,key)))
     (plist-put ,plist ,key ,form)))

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

(defun org-sql--to-tree-config (file-hash paths-with-attributes log-note-headings
                                     todo-keywords lb-config size lines tree)
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
          :size size
          :lines lines
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

(defun org-sql--to-hstate (headline-id tree-config headline)
  "Return new hstate set from TREE-CONFIG and HEADLINE.

An HSTATE represents the current headline being processed and
will include the follwing keys/values:
- `:file-hash' the path to the current file being processed
- `:lb-config' the supercontents config plist
- `:log-note-matcher': a list of log-note-matchers for this org
  file as returned by `org-sql--build-log-note-heading-matchers'
- `:headline' the current headline node."
  (-let (((&plist :file-hash h :lb-config c :log-note-matcher m) tree-config))
    (list :file-hash h
          :lb-config (org-sql--headline-update-supercontents-config c headline)
          :log-note-matcher m
          :headline headline
          :parent-ids (list headline-id))))

(defun org-sql--update-hstate (headline-id hstate headline)
  "Return a new HSTATE updated with information from HEADLINE.
Only the :lb-config and :headline keys will be changed."
  ;; TODO plist-put won't work here
  (->> (org-sql--replace-in-plist :headline headline hstate)
       (org-sql--map-plist :lb-config
         (org-sql--headline-update-supercontents-config it headline))
       (org-sql--map-plist :parent-ids (cons headline-id it))))

;; (defun org-sql--to-hashpathpair (disk-path db-path file-hash)
;;   "Return a plist representing org file status.
;; DISK-PATH is the path to the org file on disk, DB-PATH is the
;; path on disk recorded in the database for this org file, and
;; FILE-HASH is the md5 of this org file."
;;   (list :disk-path disk-path :db-path db-path :file-hash file-hash))

(defun org-sql--to-hashpathpair (file-path file-hash)
  (list :file-path file-path :file-hash file-hash))

;;; SQL string parsing functions

(defun org-sql--parse-output-to-list (config out)
  (unless (equal out "")
    (let ((sep (org-sql--case-mode config
                 (mysql "\t")
                 ((pgsql sqlite sqlserver) "|"))))
      (->> (s-trim out)
           (s-split "\n")
           (--map (-map #'s-trim (s-split sep it)))))))

(defun org-sql--parse-output-to-plist (config cols out)
  "Parse SQL output string OUT to an plist representing the data.
COLS are the column names as symbols used to obtain OUT.
CONFIG is the `org-sql-db-config' list."
  (-some->> (org-sql--parse-output-to-list config out)
    (--map (-interleave cols it))))

;;; MQL -> SQL string formatting functions

;; formatting function tree

(defun org-sql--compile-mql-format-function (config type)
  "Return SQL value formatting function.
The returned function will depend on the MODE and TYPE."
  (let ((formatter-form
         (org-sql--case-type type
           (boolean
            (org-sql--case-mode config
              ((mysql pgsql)
               '(if (= it 1) "TRUE" "FALSE"))
              ((sqlite sqlserver)
               '(if (= it 1) "1" "0"))))
           (enum
            '(format "'%s'" (symbol-name it)))
           ((integer real)
            '(number-to-string it))
           ((char text varchar)
            (let ((escaped-chars
                   (org-sql--case-mode config
                     (mysql '(("'" . "\\\\'")
                              ("\n" . "\\\\n")))
                     ;; TODO not sure if these also needs Char(13) in front of
                     ;; Char(10) for the carriage return if using on windows
                     ;; (alas...newline war)
                     (pgsql '(("'" . "''")
                              ("\n" . "'||chr(10)||'")))
                     (sqlite '(("'" . "''")
                               ("\n" . "'||char(10)||'")))
                     (sqlserver '(("'" . "''")
                                  ("\n" . "+Char(10)+"))))))
              `(->> ',escaped-chars
                    (--reduce-from (s-replace (car it) (cdr it) acc) it)
                    (format "'%s'")))))))
    `(lambda (it) (if it ,formatter-form "NULL"))))

(defun org-sql--get-column-formatter (config tbl-name column-name)
  (let ((column (->> org-sql--mql-tables
                     (alist-get tbl-name)
                     (alist-get 'columns)
                     (alist-get column-name))))
    (org-sql--compile-mql-format-function config (plist-get column :type))))

;; (defun org-sql--compile-mql-schema-formatter-alist (config mql-table)
;;   "Return an alist of formatting functions for MQL-TABLES.
;; MODE is the SQL mode. The alist will mirror MSL schema except that the
;; car for each column will be a formatting function."
;;   (cl-flet
;;       ((get-type-function
;;         (mql-column)
;;         (-let* (((name . (&plist :type)) mql-column))
;;           (cons name (org-sql--compile-mql-format-function config type)))))
;;     (-let* (((tbl-name . (&alist 'columns)) mql-table)
;;             (tbl-name* (org-sql--format-mql-table-name config tbl-name)))
;;       (cons tbl-name (list :table-name tbl-name*
;;                            :column-formatters (-map #'get-type-function columns))))))

;; helper functions

(defun org-sql--format-mql-plist (formatter-list sep plist)
  "Format a PLIST to a SQL-compliant string.
FORMATTER-ALIST is an alist of formatting functions matching the keys
in PLIST (whose keys in turn should match columns in the schema).
The keys and values will be formatted like \"key=val\" and
separated by SEP."
  (let ((keys (->> (-slice plist 0 nil 2)
                   (-map #'org-sql--format-mql-column-name)))
        (vals (->> (-partition 2 plist)
                   (--map (funcall
                           (->> (plist-get formatter-list :column-formatters)
                                (alist-get (car it)))
                           (cadr it))))))
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
    ((pgsql sqlserver)
     (-let (((&plist :schema) (cdr config)))
       (if (not schema) (symbol-name tbl-name)
         (format "%s.%s" schema tbl-name))))))

(defun org-sql--format-mql-enum-name (config enum-name)
  "Return ENUM-NAME as a formatted string according to CONFIG."
  ;; ASSUME only modes that support ENUM will call this
  (-let (((&plist :schema) (cdr config)))
    (if (not schema) enum-name (format "%s.%s" schema enum-name))))

;; create table

;; (defun org-sql--format-mql-schema-enum-types (config mql-tables)
;;   "Return a series of CREATE TYPE statements for MQL-SCHEMA.
;; The SQL statements will create all enum types found in
;; MQL-TABLES. CONFIG is the `org-sql-db-config'."
;;   ;; ASSUME only modes that support ENUM will call this
;;   (cl-labels
;;       ((format-column
;;         (tbl-name mql-column)
;;         (-let* (((column-name . (&plist :type :type-id :allowed)) mql-column)
;;                 (column-name* (org-sql--format-mql-column-name column-name)))
;;           (when (and (eq type 'enum) allowed)
;;             (let ((type-name
;;                    (->> (or type-id (format "enum_%s_%s" tbl-name column-name*))
;;                         (org-sql--format-mql-enum-name config))))
;;               (->> (--map (format "'%s'" it) allowed)
;;                    (s-join ",")
;;                    (format "CREATE TYPE %s AS ENUM (%s);" type-name))))))
;;        (format-table
;;         (mql-table)
;;         (-let (((table-name . (&alist 'columns)) mql-table))
;;           (--map (format-column table-name it) columns))))
;;     (->> (-mapcat #'format-table mql-tables)
;;          (-non-nil)
;;          (-uniq))))

(defun org-sql--get-enum-type-names (config mql-tables)
  (cl-flet
      ((flatten
        (tbl)
        (let ((tbl-name (car tbl)))
          (--map (cons tbl-name it) (alist-get 'columns (cdr tbl)))))
       (format-column
        (grouped)
        (-let (((tbl-name . (col-name . (&plist :type :type-id :allowed))) grouped))
          (when (and (eq type 'enum) allowed)
            (cons (->> (or type-id
                           (->> (org-sql--format-mql-column-name col-name)
                                (format "enum_%s_%s" tbl-name)))
                       (org-sql--format-mql-enum-name config))
                  (--map (format "'%s'" it) allowed))))))
    (->> (-mapcat #'flatten mql-tables)
         (-map #'format-column)
         (-non-nil)
         (-uniq))))

(defun org-sql--format-mql-schema-enum-types (config mql-tables)
  (->> (org-sql--get-enum-type-names config mql-tables)
       (--map (format "CREATE TYPE %s AS ENUM (%s);"
                      (car it)
                      (s-join "," (cdr it))))))

(defun org-sql--format-mql-schema-column-constraints (mql-column-constraints)
  "Return formatted column constraints for MQL-COLUMN-CONSTRAINTS."
  (cl-flet
      ((format-constraint
        (constraint)
        (pcase constraint
          ('notnull "NOT NULL")
          ('unique "UNIQUE")
          (e (error "Unknown constraint %s" e)))))
    (s-join " " (-map #'format-constraint mql-column-constraints))))

(defun org-sql--format-mql-schema-type (config tbl-name mql-column)
  "Return SQL string for the type of MQL-COLUMN.
CONFIG is the `org-sql-db-config' list and TBL-NAME is the name
of the table."
  (-let* (((column-name . (&plist :type :type-id)) mql-column)
          (column-name* (org-sql--format-mql-column-name column-name)))
    ;; TODO use nvarchar(max) for text and just nvarchar in sql server
    (org-sql--case-type type
      (boolean
       (org-sql--case-mode config
         ((mysql pgsql) "BOOLEAN")
         (sqlite "INTEGER")
         (sqlserver "BIT")))
      (char
       (org-sql--case-mode config
         ((mysql sqlserver)
          (-let (((&plist :length) (cdr mql-column)))
            (if length (format "CHAR(%s)" length) "CHAR")))
         ;; pgsql should use TEXT here because (according to the docs) "there
         ;; is no performance difference among char/varchar/text except for the
         ;; length-checking"
         ((pgsql sqlite) "TEXT")))
      (enum
       (org-sql--case-mode config
         (mysql (->> (plist-get (cdr mql-column) :allowed)
                     (--map (format "'%s'" it))
                     (s-join ",")
                     (format "ENUM(%s)")))
         (pgsql (if type-id type-id
                  (->> (format "enum_%s_%s" tbl-name column-name*)
                       (org-sql--format-mql-enum-name config))))
         (sqlite "TEXT")
         (sqlserver (-if-let (length (plist-get (cdr mql-column) :length))
                        (format "VARCHAR(%s)" length)
                      "TEXT"))))
      (integer
       "INTEGER")
      (real
       (org-sql--case-mode config
         (mysql "FLOAT")
         ((pgsql sqlite sqlserver) "REAL")))
      (text
       "TEXT")
      (varchar
       (org-sql--case-mode config
         ((mysql sqlserver)
          (-let (((&plist :length) (cdr mql-column)))
            (if length (format "VARCHAR(%s)" length) "VARCHAR")))
         ;; see above why pgsql uses TEXT here
         ((pgsql sqlite) "TEXT"))))))

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
        (-let* (((&plist :ref :keys :parent-keys :on-delete) keyvals)
                (ref* (org-sql--format-mql-table-name config ref))
                (keys* (->> keys (-map #'org-sql--format-mql-column-name) (s-join ",")))
                (parent-keys* (->> parent-keys
                                   (-map #'org-sql--format-mql-column-name)
                                   (s-join ",")))
                (foreign-str (format "FOREIGN KEY (%s) REFERENCES %s (%s)"
                                     keys* ref* parent-keys*))
                (on-delete* (-some--> on-delete
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
                   ((pgsql sqlite) t)))
          (fmt (org-sql--case-mode config
                 ((mysql sqlite)
                  "CREATE TABLE IF NOT EXISTS %s (%s);")
                 (pgsql
                  (org-sql--with-config-keys (:unlogged) config
                    (s-join " " (list "CREATE"
                                      (if unlogged "UNLOGGED TABLE" "TABLE")
                                      "IF NOT EXISTS %s (%s);"))))
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
                            (--map (org-sql--format-mql-schema-table config it)))))
                            ;; (s-join ""))))
    (org-sql--case-mode config
      (pgsql
       (let ((create-types (->> (org-sql--format-mql-schema-enum-types config mql-tables))))
                                ;; (s-join ""))))
         ;; (concat create-types create-tables)))
         (append create-types create-tables)))
      ((mysql sqlite sqlserver)
       create-tables))))

;; insert

(defun org-sql--format-mql-bulk-insert (config mql-bulk-insert)
  (cl-flet
      ((format-row
        (formatters row)
        (->> (--zip-with (funcall other it) row formatters)
             (s-join ",")
             (format "(%s)"))))
    (-let* (((tbl-name . rows) mql-bulk-insert)
            (tbl-name* (org-sql--format-mql-table-name config tbl-name))
            (mql-tbl (alist-get tbl-name org-sql--mql-tables))
            (mql-columns (alist-get 'columns mql-tbl))
            (columns* (->> mql-columns
                          ;; TODO there is a better function for this
                          (--map (org-sql--format-mql-column-name (car it)))
                          (s-join ",")))
            ;; ASSUME these will be in the right order
            (formatters (--map (->> (plist-get (cdr it) :type)
                                    (org-sql--compile-mql-format-function config))
                               mql-columns)))
      (->> (--map (format-row formatters it) rows)
           (s-join ",")
           (format "INSERT INTO %s (%s) VALUES %s;" tbl-name* columns*)))))

(defun org-sql--format-mql-bulk-inserts (config mql-bulk-inserts)
  (->> (-filter #'cdr mql-bulk-inserts)
       (--map (org-sql--format-mql-bulk-insert config it))
       (s-join "")))

;; select

(defun org-sql--format-select-statement (config columns tbl-name)
  (let ((tbl-name* (org-sql--format-mql-table-name config tbl-name))
        (columns* (or (-some->> (-map #'org-sql--format-mql-column-name columns)
                        (s-join ","))
                      "*")))
    (format "SELECT %s FROM %s;" columns* tbl-name*)))

;; hashpathpairs/tree-config -> SQL statements

(defun org-sql--format-path-delete-statement (config hashpathpairs)
  (when hashpathpairs
    (cl-flet
        ((format-where-clause
          (path-fmtr hash-fmtr grouped)
          (-let* (((hash . paths) grouped)
                  (hash* (funcall hash-fmtr hash)))
            (->> (--map (format "file_path = %s" (funcall path-fmtr it)) paths)
                 (s-join " OR ")
                 (format "(file_hash = %s AND (%s))" hash*)))))
      (let ((tbl-name* (org-sql--format-mql-table-name config 'file_metadata))
            (hash-fmtr (org-sql--get-column-formatter config 'file_metadata :file_hash))
            (path-fmtr (org-sql--get-column-formatter config 'file_metadata :file_path)))
        (->> (org-sql--group-hashpathpairs-by-hash hashpathpairs)
             (--map (format-where-clause path-fmtr hash-fmtr it))
             (s-join " OR ")
             (format "DELETE FROM %s WHERE %s;" tbl-name*))))))

(defun org-sql--format-file-delete-statement (config hashpathpairs)
  (when hashpathpairs
    (let ((tbl-name* (org-sql--format-mql-table-name config 'file_hashes))
          (fmtr (org-sql--get-column-formatter config 'file_hashes :file_hash)))
        (->> (org-sql--hashpathpairs-to-hashes hashpathpairs)
             (--map (funcall fmtr it))
             (s-join ",")
             (format "DELETE FROM %s WHERE file_hash IN (%s);" tbl-name*)))))

(defun org-sql--init-acc ()
  (list :inserts (-clone org-sql--empty-mql-bulk-insert)
        :headline-id 1
        :timestamp-id 1
        :entry-id 1
        :link-id 1
        :property-id 1
        :clock-id 1))

(defun org-sql--acc-get (key acc)
  (plist-get acc key))

(defun org-sql--acc-incr (key acc)
  (plist-put acc key (1+ (plist-get acc key))))

(defun org-sql--acc-reset (key acc)
  (plist-put acc key 0))

(defun org-sql--format-insert-statements (config path-hashpathpairs file-tree-configs)
  ;; (let* ((acc (-clone org-sql--empty-mql-bulk-insert))
  (let* ((acc (org-sql--init-acc))
         (acc* (--reduce-from (org-sql--tree-config-to-mql-insert acc it) acc file-tree-configs)))
    (--> path-hashpathpairs
         ;; TODO this caaaddddaaddar stuff is confusing AF...
         (--reduce-from (org-sql--add-mql-insert-file-metadata* acc (cadr it) (car it) (cddr it)) acc* it)
         (plist-get it :inserts)
         (org-sql--format-mql-bulk-inserts config it))))

;;; SQL string -> SQL string formatting functions

(defun org-sql--format-sql-transaction (config sql-statements)
  "Return SQL string for a transaction.
SQL-STATEMENTS is a list of SQL statements to be included in the

transaction. MODE is the SQL mode."
  ;; TODO might want to add performance options here
  (let ((fmt (org-sql--case-mode config
               (sqlite "PRAGMA foreign_keys = ON;BEGIN;%sCOMMIT;")
               ((mysql pgsql) "BEGIN;%sCOMMIT;")
               (sqlserver "BEGIN TRANSACTION;%sCOMMIT;"))))
    (-some->> sql-statements
      (s-join "")
      (format fmt))))

;;; org-element/org-ml wrapper functions

;; TODO use org-ml-get-parents for this
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

;; (defun org-sql--headline-get-contents (headline)
;;   "Return the contents of HEADLINE.
;; This includes everything in the headline's section element that
;; is not the planning, logbook drawer, or property drawer."
;;   (-some->> (org-ml-headline-get-section headline)
;;     ;; TODO need a function in org-ml that returns non-meta
;;     ;; TODO this only works when `org-log-into-drawer' is defined
;;     (--remove (org-ml-is-any-type '(planning property-drawer) it))
;;     (--remove (and (org-ml-is-type 'drawer it)
;;                    (equal (org-element-property :drawer-name it)
;;                           org-log-into-drawer)))))

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
                                (eq (org-ml-get-property :begin it) ts-offset)))))))
         (get-substring
          (match-bounds)
          (when match-bounds
            (-let (((begin . end) match-bounds))
              (substring header-text begin end)))))
      (let ((old-ts (get-timestamp-node old-state))
            (new-ts (get-timestamp-node new-state)))
        (org-sql--to-entry type
          :file-hash file-hash
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
  (-let (((entry-type . (&plist :header-text :note-text :file-hash :ts)) entry))
    (org-sql--add-mql-insert acc logbook_entries
      :entry_id (org-sql--acc-get :entry-id acc)
      :headline_id (org-sql--acc-get :headline-id acc)
      :entry_type (symbol-name entry-type)
      :time_logged (-some->> ts
                     (org-ml-timestamp-get-start-time)
                     (org-ml-time-to-unixtime))
      :header header-text
      :note note-text)))

(defun org-sql--add-mql-insert-state-change (acc entry)
  "Add MQL-insert for state change ENTRY to ACC."
  (-let (((&plist :old-state :new-state) (cdr entry)))
    (--> (org-sql--add-mql-insert-headline-logbook-item acc entry)
         (org-sql--add-mql-insert it state_changes
           :entry_id (org-sql--acc-get :entry-id acc)
           :state_old old-state
           :state_new new-state))))

(defun org-sql--add-mql-insert-planning-change (acc hstate entry)
  "Add MQL-insert for planning change ENTRY to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let (((&plist :old-ts) (cdr entry)))
    (--> (org-sql--add-mql-insert-headline-logbook-item acc entry)
         (org-sql--add-mql-insert-timestamp it hstate old-ts)
         (org-sql--add-mql-insert it planning_changes
           :entry_id (org-sql--acc-get :entry-id acc)
           :timestamp_id (org-sql--acc-get :timestamp-id acc))
         (org-sql--acc-incr :timestamp-id it))))

(defun org-sql--add-mql-insert-headline-logbook-items (acc hstate logbook)
  "Add MQL-inserts for LOGBOOK to ACC.
LOGBOOK is the logbook value of the supercontents list returned
by `org-ml-headline-get-supercontents'. HSTATE is a plist as
returned by `org-sql--to-hstate'."
  ;; TODO what about unknown stuff?
  (-let (((&plist :headline) hstate))
    (cl-flet
        ((add-entry
          (acc entry)
          (let ((entry-type (car entry)))
            (cond
             ((memq entry-type org-sql-excluded-logbook-types)
              acc)
             ((memq entry-type '(redeadline deldeadline reschedule delschedule))
              (org-sql--add-mql-insert-planning-change acc hstate entry))
             ((eq entry-type 'state)
              (org-sql--add-mql-insert-state-change acc entry))
             (t
              (org-sql--add-mql-insert-headline-logbook-item acc entry))))))
      (->> (org-ml-logbook-get-items logbook)
           (--map (org-sql--item-to-entry hstate it))
           (--reduce-from (org-sql--acc-incr :entry-id (add-entry acc it)) acc)))))

(defun org-sql--add-mql-insert-clock (acc hstate clock note-text)
  "Add MQL-insert for CLOCK to ACC.
NOTE-TEXT is either a string or nil representing the clock-note.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let (((&plist :headline) hstate)
         (value (org-ml-get-property :value clock)))
    (--> (org-sql--add-mql-insert acc clocks
           :clock_id (org-sql--acc-get :clock-id acc)
           :headline_id (org-sql--acc-get :headline-id acc)
           :time_start (-some-> value
                         (org-ml-timestamp-get-start-time)
                         (org-ml-time-to-unixtime))
           :time_end (-some-> value
                       (org-ml-timestamp-get-end-time)
                       (org-ml-time-to-unixtime))
           :clock_note (unless org-sql-exclude-clock-notes note-text))
      (org-sql--acc-incr :clock-id it))))

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
            ((&plist :headline :file-hash) hstate))
      (cl-flet
          ((is-ignored
            (node-property)
            (member (org-ml-get-property :key node-property) ignore-list))
           (add-property
            (acc np)
            (--> (org-sql--add-mql-insert acc properties
                   :file_hash file-hash
                   :property_id (org-sql--acc-get :property-id acc)
                   :key_text (org-ml-get-property :key np)
                   :val_text (org-ml-get-property :value np))
              (org-sql--add-mql-insert it headline_properties
                :headline_id (org-sql--acc-get :headline-id acc)
                :property_id (org-sql--acc-get :property-id acc))
              (org-sql--acc-incr :property-id it))))
        (->> (org-ml-headline-get-node-properties headline)
             (-remove #'is-ignored)
             (-reduce-from #'add-property acc))))))

(defun org-sql--add-mql-insert-headline-tags (acc hstate)
  "Add MQL-insert for each tag in the current headline to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (if (eq 'all org-sql-excluded-tags) acc
    (-let (((&plist :headline) hstate))
      (cl-flet
          ((add-tag
            (acc tag inherited)
            (org-sql--add-mql-insert acc headline_tags
              :headline_id (org-sql--acc-get :headline-id acc)
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
    (-let* (((&plist :headline) hstate)
            (links (->> (--mapcat (org-ml-match '(:any * link) it) contents)
                        (--remove (member (org-ml-get-property :type it)
                                          org-sql-excluded-link-types)))))
      (cl-flet
          ((add-link
            (acc link)
            (--> (org-sql--add-mql-insert acc links
                   :link_id (org-sql--acc-get :link-id acc)
                   :headline_id (org-sql--acc-get :headline-id acc)
                   :link_path (org-ml-get-property :path link)
                   :link_text (->> (org-ml-get-children link)
                                   (-map #'org-ml-to-string)
                                   (s-join ""))
                   :link_type (org-ml-get-property :type link))
              (org-sql--acc-incr :link-id it))))
        (-reduce-from #'add-link acc links)))))

(defmacro org-sql--add-mql-insert-timestamp-mod (acc modifier-type timestamp)
  (declare (indent 2))
  (-let (((tbl-name value-col unit-col type-col type-prop value-prop unit-prop)
          (cl-case modifier-type
            (warning (list 'timestamp_warnings
                           :warning_value
                           :warning_unit
                           :warning_type
                           :warning-type
                           :warning-value
                           :warning-unit))
            (repeater (list 'timestamp_repeaters
                            :repeater_value
                            :repeater_unit
                            :repeater_type
                            :repeater-type
                            :repeater-value
                            :repeater-unit))
            (t (error "Unknown modifier type: %s" modifier-type)))))
    `(-if-let (type (org-ml-get-property ,type-prop ,timestamp))
         (org-sql--add-mql-insert ,acc ,tbl-name
           :timestamp_id (org-sql--acc-get :timestamp-id acc)
           ,value-col (org-ml-get-property ,value-prop ,timestamp)
           ,unit-col (org-ml-get-property ,unit-prop ,timestamp)
           ,type-col type)
       ,acc)))

(defun org-sql--add-mql-insert-timestamp (acc hstate timestamp)
  "Add MQL-insert for TIMESTAMP to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (cl-flet
      ((get-resolution
        (time)
        (when time (if (org-ml-time-is-long time) 1 0))))
    (-let* ((start (org-ml-timestamp-get-start-time timestamp))
            (end (org-ml-timestamp-get-end-time timestamp))
            ((&plist :headline) hstate))
      (--> acc
        (org-sql--add-mql-insert it timestamps
          :timestamp_id (org-sql--acc-get :timestamp-id acc)
          :headline_id (org-sql--acc-get :headline-id acc)
          :is_active (if (org-ml-timestamp-is-active timestamp) 1 0)
          :time_start (org-ml-time-to-unixtime start)
          :start_is_long (get-resolution start)
          :time_end (-some-> end (org-ml-time-to-unixtime))
          :end_is_long (get-resolution end)
          :raw_value (org-ml-get-property :raw-value timestamp))
        (org-sql--add-mql-insert-timestamp-mod it warning timestamp)
        (org-sql--add-mql-insert-timestamp-mod it repeater timestamp)))))

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
                (timestamps (--mapcat (org-ml-match pattern it) contents)))
          (--reduce-from (->> (org-sql--add-mql-insert-timestamp acc hstate it)
                              (org-sql--acc-incr :timestamp-id))
                         acc timestamps))
      acc)))

(defun org-sql--add-mql-insert-headline-planning (acc hstate)
  "Add MQL-insert for each planning timestamp in the current headline to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let (((&plist :headline) hstate))
    (-if-let (planning (org-ml-headline-get-planning headline))
        (cl-flet
            ((add-planning-maybe
              (acc type)
              (-if-let (ts (org-ml-get-property type planning))
                  (--> (org-sql--add-mql-insert-timestamp acc hstate ts)
                    (org-sql--add-mql-insert it planning_entries
                      :headline_id (org-sql--acc-get :headline-id acc)
                      :planning_type (->> (symbol-name type)
                                          (s-chop-prefix ":")
                                          (intern))
                      :timestamp_id (org-sql--acc-get :timestamp-id acc))
                    (org-sql--acc-incr :timestamp-id it))
                acc)))
          (--> '(:closed :deadline :scheduled)
            (-difference it org-sql-excluded-headline-planning-types)
            (-reduce-from #'add-planning-maybe acc it)))
      acc)))

(defun org-sql--add-mql-insert-headline-closures (acc hstate)
  "Add MQL-insert for parent closures from the current headline to ACC.
HSTATE is a plist as returned by `org-sql--to-hstate'."
  (-let* (((&plist :headline :parent-ids) hstate)
          (headline-id (org-sql--acc-get :headline-id acc)))
    (cl-flet
        ((add-closure
          (acc parent-id depth)
          (org-sql--add-mql-insert acc headline_closures
            :headline_id headline-id
            :parent_id parent-id
            :depth depth)))
      (->> (--map-indexed (list it it-index) parent-ids)
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
            (sc (-some->> (org-ml-headline-get-statistics-cookie headline)
                  (org-ml-get-property :value)))
            ((sc-value sc-type) (pcase sc
                                  (`(,n ,d) `(,(/ (* 1.0 n) d) fraction))
                                  (`(,p) `(,p percent))))
            (contents (org-ml-supercontents-get-contents supercontents)))
      (--> (org-sql--add-mql-insert acc headlines
             :headline_id (org-sql--acc-get :headline-id acc)
             :file_hash file-hash
             :headline_text (org-ml-get-property :raw-value headline)
             :keyword (org-ml-get-property :todo-keyword headline)
             :effort (-> (org-ml-headline-get-node-property "Effort" headline)
                         (effort-to-int))
             :priority (-some->> (org-ml-get-property :priority headline)
                         (byte-to-string))
             :stats_cookie_type sc-type
             :stats_cookie_value sc-value
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
        (org-sql--add-mql-insert-headline-logbook-items it hstate logbook)
        (org-sql--add-mql-insert-headline-closures it hstate)
        (org-sql--acc-incr :headline-id it)))))

(defun org-sql--add-mql-insert-headlines (acc tree-config)
  "Add MQL-insert headlines in TREE-CONFIG to ACC.
TREE-CONFIG is a list given by `org-sql--to-tree-config'."
  (-let (((&plist :headlines) tree-config))
    (cl-labels
        ((add-headline
          (acc hstate hl)
          (let* ((sub (org-ml-headline-get-subheadlines hl))
                 (headline-id (org-sql--acc-get :headline-id acc))
                 (hstate* (if hstate (org-sql--update-hstate headline-id hstate hl)
                            (org-sql--to-hstate headline-id tree-config hl))))
            (if (and org-sql-exclude-headline-predicate
                     (funcall org-sql-exclude-headline-predicate hl))
                acc
              (--> (org-sql--add-mql-insert-headline acc hstate*)
                (--reduce-from (add-headline acc hstate* it) it sub))))))
      (--reduce-from (add-headline acc nil it) acc headlines))))

(defun org-sql--add-mql-insert-file-tags (acc tree-config)
  "Add MQL-insert for each file tag in file to ACC.
TREE-CONFIG is a list given by `org-sql--to-tree-config'."
  (-let (((&plist :file-hash :top-section) tree-config))
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

(defun org-sql--add-mql-insert-file-properties (acc tree-config)
  "Add MQL-insert for each file property in file to ACC.
TREE-CONFIG is a list given by `org-sql--to-tree-config'."
  (-let (((&plist :file-hash :top-section) tree-config))
    (cl-flet
        ((add-property
          (acc keyword)
          (-let (((key value) (--> (org-ml-get-property :value keyword)
                                   (s-split-up-to " " it 1))))
            (--> (org-sql--add-mql-insert acc properties
                   :file_hash file-hash
                   :property_id (org-sql--acc-get :property-id acc)
                   :key_text key
                   :val_text value)
                 (org-sql--acc-incr :property-id it)))))
      (->> (--filter (org-ml-is-type 'keyword it) top-section)
           (--filter (equal (org-ml-get-property :key it) "PROPERTY"))
           (-reduce-from #'add-property acc)))))

(defun org-sql--add-mql-insert-file-hash (acc tree-config)
  "Add MQL-insert for file in TREE-CONFIG to ACC.
TREE-CONFIG is a list given by `org-sql--to-tree-config'."
  (-let (((&plist :file-hash :size :lines) tree-config))
    (org-sql--add-mql-insert acc file_hashes
      :file_hash file-hash
      :tree_size size
      :tree_lines lines)))

(defun org-sql--add-mql-insert-file-metadata* (acc path hash attrs)
  (org-sql--add-mql-insert acc file_metadata
    :file_hash hash
    :file_path path
    :file_uid (file-attribute-user-id attrs)
    :file_gid (file-attribute-group-id attrs)
    :file_modification_time (->> (file-attribute-modification-time attrs)
                                 (float-time)
                                 (round))
    :file_attr_change_time (->> (file-attribute-status-change-time attrs)
                                 (float-time)
                                 (round))
    :file_modes (file-attribute-modes attrs)))

(defun org-sql--add-mql-insert-file-metadata (acc tree-config)
  "Add MQL-insert for file in TREE-CONFIG to ACC.
TREE-CONFIG is a list given by `org-sql--to-tree-config'."
  (-let (((&plist :paths-with-attributes :file-hash) tree-config))
    (--reduce-from (org-sql--add-mql-insert-file-metadata* acc (car it) file-hash (cdr it))
                   acc paths-with-attributes)))

(defun org-sql--tree-config-to-mql-insert (acc tree-config)
  "Return all MQL-inserts for TREE-CONFIG.
TREE-CONFIG is a list given by `org-sql--to-tree-config'."
  (-> acc
      (org-sql--add-mql-insert-file-hash tree-config)
      (org-sql--add-mql-insert-file-metadata tree-config)
      (org-sql--add-mql-insert-file-properties tree-config)
      (org-sql--add-mql-insert-file-tags tree-config)
      (org-sql--add-mql-insert-headlines tree-config)))

;; (defun org-sql--hashpathpair-to-mql-delete (file-hash)
;;   (org-sql--mql-delete file_hashes (:file_hash file-hash)))

;; (defun org-sql--hashpathpair-to-mql-path-delete (file-hash file-paths)
;;   (->> (--map (list file-hash it) file-paths)
;;        (org-sql--mql-bulk-delete file_metadata (:file_path :file_hash))))
;;   ;; (--map (org-sql--mql-delete file_metadata (:file_path it :file_hash file-hash))
;;   ;;        file-paths))

;; (defun org-sql--hashpathpair-to-mql-path-insert (file-hash file-paths)
;;   (--map (org-sql--mql-insert file_metadata
;;            :file_path it
;;            :file_hash file-hash)
;;          file-paths))

;; hashpathpair function

(defun org-sql--partition-hashpathpairs (disk-hashpathpairs db-hashpathpairs)
  (let (hash-in-db paths-dont-match files-to-insert paths-to-insert
                   paths-to-delete cur-disk cur-db n)
    (while disk-hashpathpairs
      (setq hash-in-db nil
            paths-dont-match nil
            n (length db-hashpathpairs)
            cur-disk (car disk-hashpathpairs))
      (while (< 0 n)
        (setq cur-db (nth (1- n) db-hashpathpairs))
        ;; first test if hashes are equal in the two pairs
        (when (equal (car cur-disk) (car cur-db))
          ;; if yes, test if the paths are equal
          (when (not (equal (cdr cur-disk) (cdr cur-db)))
            (setq paths-dont-match t
                  paths-to-delete (cons cur-db paths-to-delete)))
          (setq hash-in-db t
                db-hashpathpairs (-remove-at (1- n) db-hashpathpairs)))
        (setq n (1- n)))
      (if hash-in-db
          (when paths-dont-match
            (setq paths-to-insert (cons cur-disk paths-to-insert)))
        (setq files-to-insert (cons cur-disk files-to-insert)))
      (setq disk-hashpathpairs (cdr disk-hashpathpairs)))
    `((files-to-insert ,@files-to-insert)
      (paths-to-insert ,@paths-to-insert)
      (paths-to-delete ,@paths-to-delete)
      (files-to-delete ,@db-hashpathpairs))))

(defun org-sql--group-hashpathpairs-by-hash (hashpathpairs)
  (--map (cons (car it) (-map #'cdr (cdr it))) (-group-by #'car hashpathpairs)))

(defun org-sql--hashpathpairs-to-hashes (hashpathpairs)
  (-map #'car hashpathpairs))

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

(defmacro org-sql--on-success* (first-form &rest success-forms)
  "Run form on successful exit code.
This is like `org-sql--on-success' but with '(error it-out)'
supplied for ERROR-FORM. FIRST-FORM and SUCCESS-FORM have the
same meaning."
  (declare (indent 1))
  `(org-sql--on-success ,first-form (progn ,@success-forms) (error it-out)))

;;;
;;; STATEFUL FUNCTIONS
;;;

;;; low-level IO

(defun org-sql--run-command (path args async)
  "Execute PATH with ARGS.
Return a cons cell like (RETURNCODE . OUTPUT)."
  (if (not async) (org-sql--run-command* path nil args)
      (make-process :command (cons path args)
                    :buffer "*Org-SQL*"
                    :connection-type 'pipe
                    :name "org-sql-async")))

(defun org-sql--run-command* (path file args)
  "Execute PATH with ARGS and FILE routed to stdin.
Return a cons cell like (RETURNCODE . OUTPUT)."
  (with-temp-buffer
    (let ((rc (apply #'call-process path file (current-buffer) nil args)))
      (cons rc (buffer-string)))))

;; (defun org-sql--run-command-async (path args)
;;   (make-process :command (cons path args)
;;                 :buffer "*Org-SQL*"
;;                 :connection-type 'pipe
;;                 :name "org-sql-async"))
;;     ;; (process-send-string proc (concat payload "\n"))
;;     ;; (process-send-eof proc)))
    
;;; hashpathpair -> tree-config

(defun org-sql--hashpathpair-get-tree-config (file-hash file-paths)
  (let ((paths-with-attributes (--map (cons it (file-attributes it 'integer)) file-paths)))
    ;; just pick the first file path to open
    (with-current-buffer (find-file-noselect (car file-paths) t)
      (let ((tree (org-element-parse-buffer))
            (todo-keywords (-map #'substring-no-properties org-todo-keywords-1))
            (lb-config (list :log-into-drawer org-log-into-drawer
                             :clock-into-drawer org-clock-into-drawer
                             :clock-out-notes org-log-note-clock-out))
            (size (buffer-size))
            (lines (save-excursion
                     (goto-char (point-max))
                     (line-number-at-pos))))
        (org-sql--to-tree-config file-hash paths-with-attributes
                            org-log-note-headings todo-keywords lb-config
                            size lines tree)))))

;;; reading hashpathpair from external state

(defun org-sql--disk-get-hashpathpairs ()
  "Get a list of hashpathpair for org files on disk.
Each hashpathpair will have it's :db-path set to nil. Only files in
`org-sql-files' will be considered."
  (cl-flet
      ((get-md5
        (fp)
        (org-sql--on-success (org-sql--run-command "md5sum" `(,fp) nil)
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
           (--map (cons (get-md5 it) it))))))

(defun org-sql--db-get-hashpathpairs ()
  "Get a list of hashpathpair for the database.
Each hashpathpair will have it's :disk-path set to nil."
  (let* ((tbl-name 'file_metadata)
         (cols '(:file_path :file_hash))
         (cmd (org-sql--format-select-statement org-sql-db-config cols tbl-name)))
    (org-sql--on-success* (org-sql-send-sql cmd)
      (->> (s-trim it-out)
           (org-sql--parse-output-to-plist org-sql-db-config cols)
           (--map (-let (((&plist :file_hash h :file_path p) it))
                    (cons h p)))))))

(defun org-sql--get-transactions ()
  "Return SQL string of the update transaction.
This transaction will bring the database to represent the same
state as the orgfiles on disk."
  (-let* ((disk-hashpathpairs (org-sql--disk-get-hashpathpairs))
          (db-hashpathpairs (org-sql--db-get-hashpathpairs))
          ((&alist 'files-to-insert fi
                   'files-to-delete fd
                   'paths-to-insert pi
                   'paths-to-delete pd)
           (org-sql--partition-hashpathpairs disk-hashpathpairs db-hashpathpairs))
          (pi* (--map (cons (car it) (cons (cdr it) (file-attributes (cdr it)))) pi)))
    (list (org-sql--format-path-delete-statement org-sql-db-config pd)
          (org-sql--format-file-delete-statement org-sql-db-config fd)
          (->> (org-sql--group-hashpathpairs-by-hash fi)
               (--map (org-sql--hashpathpair-get-tree-config (car it) (cdr it)))
               (org-sql--format-insert-statements org-sql-db-config pi*)))))
         ;; (org-sql--format-sql-transaction org-sql-db-config))))

(defun org-sql-dump-update-transactions ()
  "Dump the update transaction to a separate buffer."
  (let ((out (->> (org-sql--get-transactions)
                  (org-sql--format-sql-transaction org-sql-db-config))))
    (switch-to-buffer "SQL: Org-update-dump")
    (insert (s-replace ";" ";\n" out))))

;;; SQL command wrappers

(defun org-sql--append-process-environment (env &rest pairs)
  (declare (indent 1))
  (->> (-partition 2 pairs)
       (--map (and (cadr it) (format "%s=%s" (car it) (cadr it))))
       (-non-nil)
       (append process-environment env)))

(defun org-sql--exec-mysql-command-nodb (fargs async)
  (org-sql--with-config-keys (:defaults :defaults-extra :hostname :port
                                        :username :password :args :env)
      org-sql-db-config
    (let ((all-args (append
                     ;; either of these must be the first option if given
                     (or (-some->> defaults
                           (format "--defaults-file=%s")
                           (list))
                         (-some->> defaults-extra
                           (format "--defaults-extra-file=%s")
                           (list)))
                     (-some->> hostname (list "-h"))
                     (-some->> port (list "-P"))
                     (-some->> username (list "-u"))
                     ;; this makes the output tidy (no headers or extra output)
                     '("-Ns")
                     args
                     fargs))
          (process-environment (org-sql--append-process-environment env
                                 "MYSQL_PWD" password)))
      (org-sql--run-command org-sql--mysql-exe all-args async))))

(defun org-sql--exec-postgres-command-nodb (fargs async)
  "Execute a postgres command with ARGS.
CONFIG-KEYS is a list like `org-sql-db-config'."
  (org-sql--with-config-keys (:service-file :pass-file :hostname :port
                                            :username :password :args :env)
      org-sql-db-config
    (let ((all-args (append
                     (-some->> hostname (list "-h"))
                     (-some->> port (list "-p"))
                     (-some->> username (list "-U"))
                     ;; don't prompt for password
                     '("-w")
                     ;; make output tidy (tabular, quiet, and no alignment)
                     '("-qAt")
                     args
                     fargs))
          (process-environment (org-sql--append-process-environment env
                                 "PGPASSWORD" password
                                 "PGSERVICEFILE" service-file
                                 "PGPASSFILE" pass-file
                                 ;; suppress warning messages
                                 "PGOPTIONS" "-c client_min_messages=WARNING")))
      (org-sql--run-command org-sql--psql-exe all-args async))))

(defun org-sql--exec-sqlserver-command-nodb (fargs async)
  (org-sql--with-config-keys (:server :username :password :args :env)
      org-sql-db-config
    (let ((all-args (append
                     (-some->> server (list "-S"))
                     (-some->> username (list "-U"))
                     ;; use pipe char as the separator
                     '("-s" "|")
                     ;; don't use headers
                     '("-h" "-1")
                     args
                     fargs))
          (process-environment (org-sql--append-process-environment env
                                 "SQLCMDPASSWORD" password)))
      (org-sql--run-command org-sql--sqlserver-exe all-args async))))

(defun org-sql--exec-sqlite-command (args async)
  "Execute a sqlite command with ARGS.
CONFIG is the plist component if `org-sql-db-config'."
  (org-sql--with-config-keys (:path) org-sql-db-config
    (if (not path) (error "No path specified")
      (org-sql--run-command org-sql--sqlite-exe (cons path args) async))))

(defun org-sql--exec-sqlserver-command (args async)
  (org-sql--with-config-keys (:database) org-sql-db-config
    (if (not database) (error "No database specified")
      (org-sql--exec-sqlserver-command-nodb `("-d" ,database ,@args) async))))

(defun org-sql--exec-command-in-db (args async)
  (cl-flet
      ((send
        (fun flag key args async)
        (-if-let (target (org-sql--get-config-key key org-sql-db-config))
            (funcall fun (if flag `(,flag ,target ,@args) (cons target args)) async)
          (error (format "%s not specified" key)))))
  (org-sql--case-mode org-sql-db-config
    (mysql
     (send #'org-sql--exec-mysql-command-nodb "-D" :database args async))
    (pgsql
     (send #'org-sql--exec-postgres-command-nodb "-d" :database args async))
    (sqlite
     (org-sql--exec-sqlite-command args async))
    (sqlserver
     (send #'org-sql--exec-sqlserver-command-nodb "-d" :database args async)))))

(defun org-sql--send-sql-file (path async)
  (org-sql--case-mode org-sql-db-config
    (mysql
     (org-sql-send-sql (format "source %s" path) async))
    (pgsql
     (org-sql-send-sql (format "\\i %s" path) async))
    (sqlite
     (org-sql-send-sql (format ".read %s" path) async))
    (sqlserver
     ;; I think ":r tmp-path" should work here to make this analogous
     ;; with the others
     (org-sql--exec-sqlserver-command `("-i" ,path) async))))

(defun org-sql--send-sql* (sql-cmd async)
  "Execute SQL-CMD as a separate file input.
The database connection will be handled transparently."
  ;; TODO I don't think there are cases where I want to send a nil cmd, so
  ;; nil should be an error
  (if (not sql-cmd) '(0 . "")
    (-let ((tmp-path (->> (round (float-time))
                          (format "%sorg-sql-cmd-%s" (temporary-file-directory))))
           (sql-cmd (org-sql--case-mode org-sql-db-config
                      ((mysql pgsql sqlite) sql-cmd)
                      (sqlserver (format "set nocount on; %s" sql-cmd)))))
      (f-write sql-cmd 'utf-8 tmp-path)
      (let ((res (org-sql--send-sql-file tmp-path async)))
        (if (not async)
            (f-delete tmp-path)
          (if (process-live-p res)
              (set-process-sentinel res (lambda (p e) (f-delete tmp-path)))
            (f-delete tmp-path)))
        res))))

(defun org-sql--send-transaction (statements)
  (->> (org-sql--format-sql-transaction org-sql-db-config statements)
       (org-sql-send-sql)))

(defun org-sql--send-transaction-with-hook (pre-key post-key trans-stmts)
  (-let* (((pre-in-trans before-trans) (-some-> pre-key (org-sql--pull-hook)))
          ((post-in-trans after-trans) (-some-> post-key (org-sql--pull-hook)))
          (ts (->> (append pre-in-trans trans-stmts post-in-trans)
                   (org-sql--format-sql-transaction org-sql-db-config)))
          (bs (-some->> before-trans (s-join "")))
          (as (-some->> after-trans (s-join ""))))
    (org-sql--send-sql* (concat bs ts as) org-sql-async)))

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

(defun org-sql-send-sql (sql-cmd &optional async)
  "Execute SQL-CMD.
The database connection will be handled transparently."
  (let ((args (org-sql--case-mode org-sql-db-config
                (mysql `("-e" ,sql-cmd))
                (pgsql `("-c" ,sql-cmd))
                (sqlite `(,sql-cmd))
                (sqlserver `("-Q" ,(format "SET NOCOUNT ON; %s" sql-cmd))))))
    (org-sql--exec-command-in-db args async)))

;; table layer

;; (defun org-sql--run-hook (q hook-def)
;;   (org-sql--on-success (pcase hook-def
;;                          (`(file ,path) (org-sql--send-sql-file path))
;;                          (`(sql ,cmd) (org-sql-send-sql cmd))
;;                          (e (error "Unknown hook definition: %s" e)))
;;     (when org-sql-debug
;;       (let ((fmt (if (equal "" it-out)
;;                      (format "%s hook %%S completed successfully" q)
;;                    (format "%s hook %%S completed with output: %s" q it-out))))
;;         (message fmt it-out)))
;;     (let ((fmt (if (equal "" it-out) (format "%s hook %%S failed" q)
;;                  (format "%s hook %%S failed with output: %s" q it-out))))
;;       (message fmt it-out))))

;; (defun org-sql--run-hooks (key)
;;   (-when-let (hooks (org-sql--get-config-key key org-sql-db-config))
;;     (when hooks
;;       (let ((qualifier (pcase key
;;                          (:post-init-hooks "Init")
;;                          (:post-update-hooks "Update")
;;                          (:post-clear-hooks "Clear")
;;                          (e (error "Unknown qualifier: %s" e)))))
;;         (--each hooks (org-sql--run-hook qualifier it))))))

(defun org-sql--pull-hook (key)
  (-some->> (org-sql--get-config-key key org-sql-db-config)
    ;; t = INSIDE
    (--map (pcase it
             (`(file ,path) (cons nil (f-read-text path)))
             (`(file+ ,path) (cons t (f-read-text path)))
             (`(sql ,cmd) (cons nil cmd))
             (`(sql+ ,cmd) (cons t cmd))
             (e (error "Unknown hook definition: %s" e))))
    (-separate #'car)
    ;; (INSIDE AFTER)
    (--map (-map #'cdr it))))

(defun org-sql--append-hook (stmts key)
  (-let (((&alist 'append a 'independent i) (org-sql--pull-hook key)))
    (list (append stmts a) (list i))))

(defun org-sql-create-tables ()
  (->> (org-sql--format-mql-schema org-sql-db-config org-sql--mql-tables)
       (org-sql--send-transaction)))

(defun org-org-sql--get-drop-table-statements (config)
  (org-sql--case-mode config
    (mysql
     (list "SET FOREIGN_KEY_CHECKS = 0;"
           (format "DROP TABLE IF EXISTS %s;" (s-join "," org-sql-table-names))
           "SET FOREIGN_KEY_CHECKS = 1;"))
    (pgsql
     (org-sql--with-config-keys (:schema) org-sql-db-config
       (let ((drop-tables (->> (if schema
                                   (--map (format "%s.%s" schema it)
                                          org-sql-table-names)
                                 org-sql-table-names)
                               (s-join ",")
                               (format "DROP TABLE IF EXISTS %s CASCADE;")))
             (drop-types (->> (org-sql--get-enum-type-names config org-sql--mql-tables)
                              (-map #'car)
                              (s-join ",")
                              (format "DROP TYPE IF EXISTS %s CASCADE;"))))
         (list drop-tables drop-types))))
    (sqlite
     (reverse (--map (format "DROP TABLE IF EXISTS %s;" it) org-sql-table-names)))
    (sqlserver
     (org-sql--with-config-keys (:schema) config
       (let ((tbl-names (--> org-sql-table-names
                          (if schema (--map (format "%s.%s" schema it) it) it)
                          (reverse it)))
             (alter-fmt (->> (list
                              "IF EXISTS"
                              "(SELECT * FROM sys.tables where name = '%1$s')"
                              "ALTER TABLE %1$s NOCHECK CONSTRAINT ALL;")
                             (s-join " "))))
         (-snoc (--map (format alter-fmt it) tbl-names)
                (format "DROP TABLE IF EXISTS %s;" (s-join "," tbl-names))))))))

(defun org-sql-drop-tables ()
  (->> (org-org-sql--get-drop-table-statements org-sql-db-config)
       (org-sql--send-transaction)))

(defun org-sql-list-tables ()
  (-let (((sql-cmd parse-fun)
          (org-sql--case-mode org-sql-db-config
            (mysql
             (list "SHOW TABLES;"
                   (lambda (s)
                     (let ((s* (s-trim s)))
                       (unless (equal s* "") (s-lines s*))))))
            (pgsql
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
                         (let ((s* (s-trim s)))
                           (unless (equal "" s*)
                             (->> (s-lines s*)
                                  (--map (-let (((s-name t-name) (s-split "|" it)))
                                           (cons (s-trim s-name) (s-trim t-name))))
                                  (--filter (equal schema* (car it)))
                                  (--map (cdr it)))))))))))))
    (org-sql--on-success* (org-sql-send-sql sql-cmd)
      (funcall parse-fun it-out))))

;; database layer

(defun org-sql-create-db ()
  (org-sql--case-mode org-sql-db-config
    ((mysql pgsql sqlserver)
     (error "Must manually create database using admin privileges"))
    (sqlite
     ;; this is a silly command that should work on all platforms (eg doesn't
     ;; require `touch' to make an empty file)
     (org-sql--on-success* (org-sql--exec-sqlite-command '(".schema") nil)
       ;; return a dummy return-code and stdout message
       '(0 . "")))))

(defun org-sql-drop-db ()
  (org-sql--case-mode org-sql-db-config
    ((mysql pgsql sqlserver)
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
         (org-sql--on-success* (org-sql--exec-mysql-command-nodb `("-e" ,cmd) nil)
           (equal database (car (s-lines it-out)))))))
    (pgsql
     (org-sql--with-config-keys (:database) org-sql-db-config
       (let ((cmd (format "SELECT 1 FROM pg_database WHERE datname='%s';" database)))
         (org-sql--on-success* (org-sql--exec-postgres-command-nodb `("-c" ,cmd) nil)
           (equal "1" (s-trim it-out))))))
    (sqlite
     (org-sql--with-config-keys (:path) org-sql-db-config
       (file-exists-p path)))
    (sqlserver
     (org-sql--with-config-keys (:database) org-sql-db-config
       ;; TODO the 'set nocount on' bit should be common to all commands
       (let ((cmd (format "SET NOCOUNT ON;SELECT 1 FROM sys.databases WHERE name = '%s';" database)))
         (org-sql--on-success* (org-sql--exec-sqlserver-command-nodb `("-Q" ,cmd) nil)
           (equal "1" (s-trim it-out))))))))

;;; composite database functions

(defun org-sql-dump-table (tbl-name &optional as-plist)
  (let ((cmd (org-sql--format-select-statement org-sql-db-config nil tbl-name)))
    (org-sql--on-success* (org-sql-send-sql cmd)
      (if as-plist
          (let ((col-names (->> (alist-get tbl-name org-sql--mql-tables)
                                (alist-get 'columns)
                                (-map #'car))))
            (org-sql--parse-output-to-plist org-sql-db-config col-names it-out))
        (org-sql--parse-output-to-list org-sql-db-config it-out)))))

(defun org-sql-init-db ()
  (org-sql--case-mode org-sql-db-config
    ((mysql pgsql sqlserver)
     nil)
    (sqlite
     (org-sql-create-db)))
  (->> (org-sql--format-mql-schema org-sql-db-config org-sql--mql-tables)
       (org-sql--send-transaction-with-hook nil :post-init-hooks)))

(defun org-sql-update-db ()
  (let ((inhibit-message t))
    (org-save-all-org-buffers))
  (->> (org-sql--get-transactions)
       (org-sql--send-transaction-with-hook nil :post-update-hooks)))

(defun org-sql-clear-db ()
  (->> (org-sql--format-mql-table-name org-sql-db-config 'file_hashes)
       (format "DELETE FROM %s;")
       (list)
       (org-sql--send-transaction-with-hook nil :post-clear-hooks)))

(defun org-sql-reset-db ()
  (org-sql--case-mode org-sql-db-config
    ((mysql pgsql sqlserver)
     nil)
    (sqlite
     (org-sql-drop-db)))
  (let ((drop-tbl-stmts
         (org-sql--case-mode org-sql-db-config
           ((mysql pgsql sqlserver)
            (org-org-sql--get-drop-table-statements org-sql-db-config))
           (sqlite nil)))
        (init-stmts
         (org-sql--format-mql-schema org-sql-db-config org-sql--mql-tables)))
    (->> (append drop-tbl-stmts init-stmts)
         (org-sql--send-transaction-with-hook :pre-reset-hooks :post-init-hooks))))

;;; interactive functions

(defun org-sql-user-update ()
  "Update the Org SQL database."
  (interactive)
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
