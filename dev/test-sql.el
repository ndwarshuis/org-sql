(require 'org-sql)
(require 's)
(require 'buttercup)

;;; Code:

(defmacro list-to-lines (in)
  "Convert IN to string.
If IN is a string, return IN. If IN is a list starting with
list then join the cdr of IN with newlines."
  (cond
   ((stringp in) in)
   ((consp in) `(s-join "\n" ,in))
   (t (error "String or list of strings expected"))))

(defun org-ts-to-unixtime (timestamp-string)
  "Convert TIMESTAMP-STRING to unixtime."
  (let ((decoded (org-parse-time-string timestamp-string)))
    (->> (-snoc decoded (current-time-zone))
         (apply #'encode-time)
         (float-time)
         (round))))

(defconst testing-filepath "/tmp/dummy")

(defmacro expect-sql* (in tbl res-form)
  `(progn
     (insert (list-to-lines ,in))
     (let ((res ,res-form))
       (expect res :to-equal ,tbl))))

(defmacro expect-sql (in tbl)
  (declare (indent 1))
  `(expect-sql* ,in ,tbl (org-sql--extract-buffer nil testing-filepath)))

(defmacro expect-sql-tbl (name in tbl)
  (declare (indent 2))
  `(expect-sql* ,in ,tbl (->> (org-sql--extract-buffer nil testing-filepath)
                              (alist-get ',name))))

(defmacro expect-sql-tbls (names in tbl)
  (declare (indent 2))
  `(expect-sql* ,in ,tbl (->> (org-sql--extract-buffer nil testing-filepath)
                              (--filter (member (car it) ',names)))))

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

  (it "concat plist"
    (expect (org-sql--plist-concat '(:one 1 :two "2" :three :thrice))
            :to-equal "one=1,two='2',three=thrice"))

  (it "format insert"
    (expect (org-sql--fmt-insert 'nombre '(:one 1 :two "2"))
            :to-equal "insert into nombre (one,two) values (1,'2');")))

(describe "SQL metalangage spec"
  (before-all
    (org-mode))

  (before-each
    (erase-buffer))

  ;; headlines table

  (it "single headline"
    (expect-sql "* headline"
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path "/"
                :headline_text "headline"
                :keyword nil
                :effort nil
                :scheduled_offset nil
                :deadline_offset nil
                :closed_offset nil
                :priority nil
                :archived nil
                :commented nil
                :content nil)))))

  (it "two headlines"
    ;; NOTE reverse order
    (expect-sql (list "* headline"
                      "* another headline")
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 12
                :tree_path "/"
                :headline_text "another headline"
                :keyword nil
                :effort nil
                :scheduled_offset nil
                :deadline_offset nil
                :closed_offset nil
                :priority nil
                :archived nil
                :commented nil
                :content nil)
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path "/"
                :headline_text "headline"
                :keyword nil
                :effort nil
                :scheduled_offset nil
                :deadline_offset nil
                :closed_offset nil
                :priority nil
                :archived nil
                :commented nil
                :content nil)))))

  (it "fancy headline"
    (expect-sql (list "* TODO [#A] COMMENT another headline"
                      ":PROPERTIES:"
                      ":Effort: 0:30"
                      ":END:")
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path "/"
                :headline_text "another headline"
                :keyword "TODO"
                :effort 30
                :scheduled_offset nil
                :deadline_offset nil
                :closed_offset nil
                :priority "A"
                :archived nil
                :commented t
                :content nil)))))

  (it "nested headline"
    (expect-sql (list "* headline"
                      "** nested headline")
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 12
                :tree_path "/headline"
                :headline_text "nested headline"
                :keyword nil
                :effort nil
                :scheduled_offset nil
                :deadline_offset nil
                :closed_offset nil
                :priority nil
                :archived nil
                :commented nil
                :content nil)
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path "/"
                :headline_text "headline"
                :keyword nil
                :effort nil
                :scheduled_offset nil
                :deadline_offset nil
                :closed_offset nil
                :priority nil
                :archived nil
                :commented nil
                :content nil)))))


  (it "archived headline"
    (expect-sql "* headline :ARCHIVE:"
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path "/"
                :headline_text "headline"
                :keyword nil
                :effort nil
                :scheduled_offset nil
                :deadline_offset nil
                :closed_offset nil
                :priority nil
                :archived t
                :commented nil
                :content nil)))))

  (it "closed headline"
    (let* ((ts "[2112-01-01 Thu]")
           (planning (format "CLOSED: %s" ts)))
      (expect-sql (list "* headline"
                        planning)
        `((timestamp
           ,(list :file_path testing-filepath
                  :headline_offset 1
                  :timestamp_offset 20
                  :type 'inactive
                  :warning_type nil
                  :warning_value nil
                  :warning_unit nil
                  :repeat_type nil
                  :repeat_value nil
                  :repeat_unit nil
                  :time (org-ts-to-unixtime ts)
                  :resolution 'day
                  :time_end nil
                  :resolution_end nil
                  :raw_value ts))
          (headlines
           ,(list :file_path testing-filepath
                  :headline_offset 1
                  :tree_path "/"
                  :headline_text "headline"
                  :keyword nil
                  :effort nil
                  :scheduled_offset nil
                  :deadline_offset nil
                  :closed_offset 20
                  :priority nil
                  :archived nil
                  :commented nil
                  :content nil))))))

  (it "scheduled/closed/deadlined headline"
    (let* ((ts0 "<2112-01-01 Thu>")
           (ts1 "<2112-01-02 Fri>")
           (ts2 "[2112-01-03 Sat]")
           (planning (format "SCHEDULED: %s DEADLINE: %s CLOSED: %s" ts0 ts1 ts2)))
      (expect-sql (list "* headline"
                        planning)
        `((timestamp
           ,(list :file_path testing-filepath
                  :headline_offset 1
                  :timestamp_offset 75
                  :type 'inactive
                  :warning_type nil
                  :warning_value nil
                  :warning_unit nil
                  :repeat_type nil
                  :repeat_value nil
                  :repeat_unit nil
                  :time (org-ts-to-unixtime ts2)
                  :resolution 'day
                  :time_end nil
                  :resolution_end nil
                  :raw_value ts2)
           ,(list :file_path testing-filepath
                  :headline_offset 1
                  :timestamp_offset 50
                  :type 'active
                  :warning_type nil
                  :warning_value nil
                  :warning_unit nil
                  :repeat_type nil
                  :repeat_value nil
                  :repeat_unit nil
                  :time (org-ts-to-unixtime ts1)
                  :resolution 'day
                  :time_end nil
                  :resolution_end nil
                  :raw_value ts1)
           ,(list :file_path testing-filepath
                  :headline_offset 1
                  :timestamp_offset 23
                  :type 'active
                  :warning_type nil
                  :warning_value nil
                  :warning_unit nil
                  :repeat_type nil
                  :repeat_value nil
                  :repeat_unit nil
                  :time (org-ts-to-unixtime ts0)
                  :resolution 'day
                  :time_end nil
                  :resolution_end nil
                  :raw_value ts0))
          (headlines
           ,(list :file_path testing-filepath
                  :headline_offset 1
                  :tree_path "/"
                  :headline_text "headline"
                  :keyword nil
                  :effort nil
                  :scheduled_offset 23
                  :deadline_offset 50
                  :closed_offset 75
                  :priority nil
                  :archived nil
                  :commented nil
                  :content nil))))))

  ;; tags table

  (it "single tag"
    (expect-sql-tbl tags "* headline :sometag:"
      `(,(list :file_path testing-filepath
               :headline_offset 1
               :tag "sometag"
               :inherited nil))))

  (it "multiple tags"
    (expect-sql-tbl tags (list "* headline :onetag:"
                               "* headline :twotag:")
      `(,(list :file_path testing-filepath
               :headline_offset 21
               :tag "twotag"
               :inherited nil)
        ,(list :file_path testing-filepath
               :headline_offset 1
               :tag "onetag"
               :inherited nil))))

  (it "inherited tag"
    (setq org-sql-use-tag-inheritance t)
    (expect-sql-tbl tags (list "* parent :onetag:"
                               "** nested")
      `(,(list :file_path testing-filepath
               :headline_offset 19
               :tag "onetag"
               :inherited t)
        ,(list :file_path testing-filepath
               :headline_offset 1
               :tag "onetag"
               :inherited nil))))

  (it "inherited tag (ARCHIVE_ITAGS)"
    ;; TODO clean up the variable settings elsewhere
    (expect-sql-tbl tags (list "* parent"
                               ":PROPERTIES:"
                               ":ARCHIVE_ITAGS: sometag"
                               ":END:")
      `(,(list :file_path testing-filepath
               :headline_offset 1
               :tag "sometag"
               :inherited t))))

  (it "inherited tag (option off)"
    ;; TODO clean up the variable settings elsewhere
    (setq org-sql-use-tag-inheritance nil)
    (expect-sql-tbl tags (list "* parent :onetag:"
                               "** nested")
      `(,(list :file_path testing-filepath
               :headline_offset 1
               :tag "onetag"
               :inherited nil))))

 ;;  ;; timestamp table

  (it "closed timestamp"
    (let* ((ts "<2112-01-01 Thu>")
           (planning (format "CLOSED: %s" ts)))
      (expect-sql-tbl timestamp (list "* parent"
                                      planning)
        `(,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 18
                 :type 'active
                 :warning_type nil
                 :warning_value nil
                 :warning_unit nil
                 :repeat_type nil
                 :repeat_value nil
                 :repeat_unit nil
                 :time (org-ts-to-unixtime ts)
                 :resolution 'day
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts)))))

  (it "closed timestamp (long)"
    (let* ((ts "<2112-01-01 Thu 00:00>")
           (planning (format "CLOSED: %s" ts)))
      (expect-sql-tbl timestamp (list "* parent"
                                      planning)
        `(,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 18
                 :type 'active
                 :warning_type nil
                 :warning_value nil
                 :warning_unit nil
                 :repeat_type nil
                 :repeat_value nil
                 :repeat_unit nil
                 :time (org-ts-to-unixtime ts)
                 :resolution 'minute
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts)))))

 (it "timestamp deadline (repeater)"
    (let* ((ts "<2112-01-01 Thu +2d>")
           (planning (format "DEADLINE: %s" ts)))
      (expect-sql-tbl timestamp (list "* parent"
                                      planning)
        `(,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 20
                 :type 'active
                 :warning_type nil
                 :warning_value nil
                 :warning_unit nil
                 :repeat_type 'cumulate
                 :repeat_value 2
                 :repeat_unit 'day
                 :time (org-ts-to-unixtime ts)
                 :resolution 'day
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts)))))

 (it "timestamp deadline (warning)"
    (let* ((ts "<2112-01-01 Thu -2d>")
           (planning (format "DEADLINE: %s" ts)))
      (expect-sql-tbl timestamp (list "* parent"
                                      planning)
        `(,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 20
                 :type 'active
                 :warning_type 'all
                 :warning_value 2
                 :warning_unit 'day
                 :repeat_type nil
                 :repeat_value nil
                 :repeat_unit nil
                 :time (org-ts-to-unixtime ts)
                 :resolution 'day
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts)))))

 ;; TODO obviously this is bullshit
 (it "timestamp deadline (ranged)"
   (let* ((ts0 "<2112-01-01 Thu>")
          (ts1 "<2112-01-02 Fri>")
          (ts (format "%s--%s" ts0 ts1))
          (planning (format "DEADLINE: %s" ts)))
     (expect-sql-tbl timestamp (list "* parent"
                                     planning)
       `(,(list :file_path testing-filepath
                :headline_offset 1
                :timestamp_offset 20
                :type 'active
                :warning_type nil
                :warning_value nil
                :warning_unit nil
                :repeat_type nil
                :repeat_value nil
                :repeat_unit nil
                :time (org-ts-to-unixtime ts0)
                :resolution 'day
                ;; TODO this is odd
                :time_end (org-ts-to-unixtime ts1)
                :resolution_end 'day
                :raw_value ts)))))

 ;; link table

 (it "single link"
   (expect-sql-tbl links (list "* parent"
                               "https://example.com")
     `(,(list :file_path testing-filepath
              :headline_offset 1
              :link_offset 10
              :link_path "//example.com"
              :link_text ""
              :link_type "https"))))

 (it "two links"
   (expect-sql-tbl links (list "* parent"
                               "https://example.org"
                               "https://example.com")
     `(,(list :file_path testing-filepath
              :headline_offset 1
              :link_offset 30
              :link_path "//example.com"
              :link_text ""
              :link_type "https")
       ,(list :file_path testing-filepath
              :headline_offset 1
              :link_offset 10
              :link_path "//example.org"
              :link_text ""
              :link_type "https"))))
 
 (it "link with description"
   (expect-sql-tbl links (list "* parent"
                               "[[https://example.org][relevant]]")
     `(,(list :file_path testing-filepath
              :headline_offset 1
              :link_offset 10
              :link_path "//example.org"
              :link_text "relevant"
              :link_type "https"))))

 (it "file link"
   (expect-sql-tbl links (list "* parent"
                               "file:///tmp/eternalblue.exe")
     `(,(list :file_path testing-filepath
              :headline_offset 1
              :link_offset 10
              :link_path "/tmp/eternalblue.exe"
              :link_text ""
              :link_type "file"))))

 (it "single link (ignored)"
   (let ((org-sql-ignored-link-types 'all))
     (expect-sql-tbl links (list "* parent"
                                 "file:///tmp/eternalblue.exe")
       nil)))

 ;; property table

 (it "single property"
   (expect-sql-tbl properties (list "* parent"
                                    ":PROPERTIES:"
                                    ":key: val"
                                    ":END:")
     `(,(list :file_path testing-filepath
              :headline_offset 1
              :property_offset 23
              :key_text "key"
              :val_text "val"
              ;; TODO shouldn't this only be 0/1?
              :inherited nil))))

 (it "multiple properties"
   (expect-sql-tbl properties (list "* parent"
                                    ":PROPERTIES:"
                                    ":p1: ragtime dandies"
                                    ":p2: this time its personal"
                                    ":END:")
     `(,(list :file_path testing-filepath
              :headline_offset 1
              :property_offset 44
              :key_text "p2"
              :val_text "this time its personal"
              :inherited nil)
       ,(list :file_path testing-filepath
              :headline_offset 1
              :property_offset 23
              :key_text "p1"
              :val_text "ragtime dandies"
              :inherited nil))))

 ;; ;; TODO add inherited properties once they exist

 (it "single clock (closed)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts0 "[2112-01-01 Fri 00:00]")
          (ts1 "[2112-01-02 Sat 01:00]")
          (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
     (expect-sql-tbl clocking (list "* parent"
                                      ":LOGBOOK:"
                                      clock
                                      ":END:")
       ;; TODO what happens if we join tables and names collide?
       `(,(list :file_path testing-filepath
                :headline_offset 1
                :clock_offset 20
                :time_start (org-ts-to-unixtime ts0)
                :time_end (org-ts-to-unixtime ts1)
                ;; TODO why is this a ""?
                :clock_note nil)))))

 (it "single clock (open)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts "[2112-01-01 Fri 00:00]")
          (clock (format "CLOCK: %s" ts)))
     (expect-sql-tbl clocking (list "* parent"
                                      ":LOGBOOK:"
                                      clock
                                      ":END:")
       `(,(list :file_path testing-filepath
                :headline_offset 1
                :clock_offset 20
                :time_start (org-ts-to-unixtime ts)
                :time_end nil
                :clock_note nil)))))

 (it "multiple clocks"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts0 "[2112-01-01 Fri 00:00]")
          (ts1 "[2112-01-01 Fri 01:00]")
          (clock0 (format "CLOCK: %s" ts0))
          (clock1 (format "CLOCK: %s" ts1)))
     (expect-sql-tbl clocking (list "* parent"
                                      ":LOGBOOK:"
                                      clock0
                                      clock1
                                      ":END:")
       `(,(list :file_path testing-filepath
                :headline_offset 1
                :clock_offset 50
                :time_start (org-ts-to-unixtime ts1)
                :time_end nil
                :clock_note nil)
         ,(list :file_path testing-filepath
                :headline_offset 1
                :clock_offset 20
                :time_start (org-ts-to-unixtime ts0)
                :time_end nil
                :clock_note nil)))))

 (it "logbook item (note)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts "[2112-01-01 Fri 00:00]")
          (header (format "Note taken on %s" ts))
          (note "fancy note"))
     (expect-sql-tbl logbook (list "* parent"
                                    ":LOGBOOK:"
                                    (format "- %s \\\\" header)
                                    (format "  %s" note)
                                    ":END:")
       `(,(list :file_path testing-filepath
                :headline_offset 1
                :entry_offset 20
                :entry_type 'note
                :time_logged (org-ts-to-unixtime ts)
                :header header
                :note note)))))

 (it "logbook item (state change)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts "[2112-01-01 Fri 00:00]")
          (header (format "State \"DONE\"       from \"TODO\"       %s" ts)))
     (expect-sql-tbls (logbook state_changes)
         (list "* parent"
               ":LOGBOOK:"
               (format "- %s" header)
               ":END:")
       `((state_changes
          ,(list :file_path testing-filepath
                 :entry_offset 20
                 :state_old "TODO"
                 :state_new "DONE"))
         (logbook
          ,(list :file_path testing-filepath
                 :headline_offset 1
                 :entry_offset 20
                 :entry_type 'state
                 :time_logged (org-ts-to-unixtime ts)
                 :header header
                 :note nil))))))

 (it "logbook item (reschedule)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts0 "[2112-01-01 Fri 00:00]")
          (ts1 "[2112-01-01 Fri 01:00]")
          (header (format "Rescheduled from \"%s\" on %s" ts0 ts1)))
     (expect-sql-tbls (logbook timestamp planning_changes)
         (list "* parent"
               ":LOGBOOK:"
               (format "- %s" header)
               ":END:")
       `((timestamp
          ,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 40
                 :type 'inactive
                 :warning_type nil
                 :warning_value nil
                 :warning_unit nil
                 :repeat_type nil
                 :repeat_value nil
                 :repeat_unit nil
                 :time (org-ts-to-unixtime ts0)
                 :resolution 'minute
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts0))
         (planning_changes
          ,(list :file_path testing-filepath
                 :entry_offset 20
                 :timestamp_offset 40))
         (logbook
          ,(list :file_path testing-filepath
                 :headline_offset 1
                 :entry_offset 20
                 :entry_type 'reschedule
                 :time_logged (org-ts-to-unixtime ts1)
                 :header header
                 :note nil))))))

 (it "logbook item (redeadline)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts0 "[2112-01-01 Fri 00:00]")
          (ts1 "[2112-01-01 Fri 01:00]")
          (header (format "New deadline from \"%s\" on %s" ts0 ts1)))
     (expect-sql-tbls (logbook timestamp planning_changes)
         (list "* parent"
               ":LOGBOOK:"
               (format "- %s" header)
               ":END:")
       `((timestamp
          ,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 41
                 :type 'inactive
                 :warning_type nil
                 :warning_value nil
                 :warning_unit nil
                 :repeat_type nil
                 :repeat_value nil
                 :repeat_unit nil
                 :time (org-ts-to-unixtime ts0)
                 :resolution 'minute
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts0))
         (planning_changes
          ,(list :file_path testing-filepath
                 :entry_offset 20
                 :timestamp_offset 41))
         (logbook
          ,(list :file_path testing-filepath
                 :headline_offset 1
                 :entry_offset 20
                 :entry_type 'redeadline
                 :time_logged (org-ts-to-unixtime ts1)
                 :header header
                 :note nil))))))

 (it "logbook item (delschedule)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts0 "[2112-01-01 Fri 00:00]")
          (ts1 "[2112-01-01 Fri 01:00]")
          (header (format "Not scheduled, was \"%s\" on %s" ts0 ts1)))
     (expect-sql-tbls (logbook timestamp planning_changes)
         (list "* parent"
               ":LOGBOOK:"
               (format "- %s" header)
               ":END:")
       `((timestamp
          ,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 42
                 :type 'inactive
                 :warning_type nil
                 :warning_value nil
                 :warning_unit nil
                 :repeat_type nil
                 :repeat_value nil
                 :repeat_unit nil
                 :time (org-ts-to-unixtime ts0)
                 :resolution 'minute
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts0))
         (planning_changes
          ,(list :file_path testing-filepath
                 :entry_offset 20
                 :timestamp_offset 42))
         (logbook
          ,(list :file_path testing-filepath
                :headline_offset 1
                :entry_offset 20
                :entry_type 'delschedule
                :time_logged (org-ts-to-unixtime ts1)
                :header header
                :note nil))))))

 (it "logbook item (deldeadline)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts0 "[2112-01-01 Fri 00:00]")
          (ts1 "[2112-01-01 Fri 01:00]")
          (header (format "Removed deadline, was \"%s\" on %s" ts0 ts1)))
     (expect-sql-tbls (logbook timestamp planning_changes)
         (list "* parent"
               ":LOGBOOK:"
               (format "- %s" header)
               ":END:")
       `((timestamp
          ,(list :file_path testing-filepath
                 :headline_offset 1
                 :timestamp_offset 45
                 :type 'inactive
                 :warning_type nil
                 :warning_value nil
                 :warning_unit nil
                 :repeat_type nil
                 :repeat_value nil
                 :repeat_unit nil
                 :time (org-ts-to-unixtime ts0)
                 :resolution 'minute
                 :time_end nil
                 :resolution_end nil
                 :raw_value ts0))
         (planning_changes
          ,(list :file_path testing-filepath
                 :entry_offset 20
                 :timestamp_offset 45))
         (logbook
          ,(list :file_path testing-filepath
                :headline_offset 1
                :entry_offset 20
                :entry_type 'deldeadline
                :time_logged (org-ts-to-unixtime ts1)
                :header header
                :note nil))))))

 (it "logbook item (refile)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts "[2112-01-01 Fri 00:00]")
          (header (format "Refiled on %s" ts)))
     (expect-sql-tbl logbook (list "* parent"
                                    ":LOGBOOK:"
                                    (format "- %s" header)
                                    ":END:")
       `(,(list :file_path testing-filepath
                :headline_offset 1
                :entry_offset 20
                :entry_type 'refile
                :time_logged (org-ts-to-unixtime ts)
                :header header
                :note nil)))))

 (it "logbook item (done)"
   (let* ((org-log-into-drawer "LOGBOOK")
          (ts "[2112-01-01 Fri 00:00]")
          (header (format "CLOSING NOTE %s" ts)))
     (expect-sql-tbl logbook (list "* parent"
                                    ":LOGBOOK:"
                                    (format "- %s" header)
                                    ":END:")
       `(,(list :file_path testing-filepath
                :headline_offset 1
                :entry_offset 20
                :entry_type 'done
                :time_logged (org-ts-to-unixtime ts)
                :header header
                :note nil)))))

 )
