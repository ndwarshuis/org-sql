;;; org-sql-test-internal.el --- Internal tests for org-sql -*- lexical-binding: t; -*-

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

;; This spec tests the internal metalanguage (and associated functions) of
;; org-sql

;;; Code:

(require 'org-sql)
(require 's)
(require 'buttercup)

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

(defmacro expect-sql-tbls (names in tbl)
  (declare (indent 2))
  `(expect-sql* ,in ,tbl (->> (org-sql--extract-buffer nil testing-filepath)
                              (--filter (member (car it) ',names)))))

(describe "SQL metalangage spec"
  (before-all
    (org-mode))

  (before-each
    (erase-buffer))

  ;; headlines table

  (it "single headline"
    (expect-sql "* headline"
      `((headline_closures :file_path ,testing-filepath
                           :headline_offset 1
                           :parent_offset 1
                           :depth 0)
        (headlines :file_path ,testing-filepath
                   :headline_offset 1
                   :headline_text "headline"
                   :keyword nil
                   :effort nil
                   :priority nil
                   :is_archived 0
                   :is_commented 0
                   :content nil))))

  (it "two headlines"
    ;; NOTE reverse order
    (expect-sql (list "* headline"
                      "* another headline")
      `((headline_closures :file_path ,testing-filepath
                           :headline_offset 12
                           :parent_offset 12
                           :depth 0)
        (headlines :file_path ,testing-filepath
                   :headline_offset 12
                   :headline_text "another headline"
                   :keyword nil
                   :effort nil
                   :priority nil
                   :is_archived 0
                   :is_commented 0
                   :content nil)
        (headline_closures :file_path ,testing-filepath
                           :headline_offset 1
                           :parent_offset 1
                           :depth 0)
        (headlines :file_path ,testing-filepath
                   :headline_offset 1
                   :headline_text "headline"
                   :keyword nil
                   :effort nil
                   :priority nil
                   :is_archived 0
                   :is_commented 0
                   :content nil))))

  (it "fancy headline"
    (expect-sql (list "* TODO [#A] COMMENT another headline"
                      ":PROPERTIES:"
                      ":Effort: 0:30"
                      ":END:"
                      "this /should/ appear")
      `((headline_closures :file_path ,testing-filepath
                           :headline_offset 1
                           :parent_offset 1
                           :depth 0)
        (headlines :file_path ,testing-filepath
                   :headline_offset 1
                   :headline_text "another headline"
                   :keyword "TODO"
                   :effort 30
                   :priority "A"
                   :is_archived 0
                   :is_commented 1
                   :content "this /should/ appear\n"))))

  (it "nested headline"
    (expect-sql (list "* headline"
                      "** nested headline")
      `((headline_closures :file_path ,testing-filepath
                           :headline_offset 12
                           :parent_offset 12
                           :depth 0)
        (headline_closures :file_path ,testing-filepath
                           :headline_offset 12
                           :parent_offset 1
                           :depth 1)
        (headlines :file_path ,testing-filepath
                   :headline_offset 12
                   :headline_text "nested headline"
                   :keyword nil
                   :effort nil
                   :priority nil
                   :is_archived 0
                   :is_commented 0
                   :content nil)
        (headline_closures :file_path ,testing-filepath
                           :headline_offset 1
                           :parent_offset 1
                           :depth 0)
        (headlines :file_path ,testing-filepath
                   :headline_offset 1
                   :headline_text "headline"
                   :keyword nil
                   :effort nil
                   :priority nil
                   :is_archived 0
                   :is_commented 0
                   :content nil))))


  (it "archived headline"
    (expect-sql "* headline :ARCHIVE:"
      `((headline_closures :file_path ,testing-filepath
                           :headline_offset 1
                           :parent_offset 1
                           :depth 0)
        (headlines :file_path ,testing-filepath
                   :headline_offset 1
                   :headline_text "headline"
                   :keyword nil
                   :effort nil
                   :priority nil
                   :is_archived 1
                   :is_commented 0
                   :content nil))))

  (it "closed headline"
    (let* ((ts "[2112-01-01 Thu]"))
      (expect-sql (list "* headline"
                        (format "CLOSED: %s" ts))
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 20
                      :is_active 0
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts)
                      :start_is_long 0
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts)
          (planning_entries :file_path ,testing-filepath
                            :headline_offset 1
                            :planning_type closed
                            :timestamp_offset 20)
          (headline_closures :file_path ,testing-filepath
                             :headline_offset 1
                             :parent_offset 1
                             :depth 0)
          (headlines :file_path ,testing-filepath
                     :headline_offset 1
                     :headline_text "headline"
                     :keyword nil
                     :effort nil
                     :priority nil
                     :is_archived 0
                     :is_commented 0
                     :content nil)))))

  (it "scheduled/closed/deadlined headline"
    (let ((ts0 "<2112-01-01 Thu>")
          (ts1 "<2112-01-02 Fri>")
          (ts2 "[2112-01-03 Sat]"))
      (expect-sql
          (list "* headline"
                (format "SCHEDULED: %s DEADLINE: %s CLOSED: %s" ts0 ts1 ts2))
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 23
                      :is_active 1
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts0)
                      :start_is_long 0
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts0)
          (planning_entries :file_path ,testing-filepath
                            :headline_offset 1
                            :planning_type scheduled
                            :timestamp_offset 23)
          (timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 50
                      :is_active 1
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts1)
                      :start_is_long 0
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts1)
          (planning_entries :file_path ,testing-filepath
                            :headline_offset 1
                            :planning_type deadline
                            :timestamp_offset 50)
          (timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 75
                      :is_active 0
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts2)
                      :start_is_long 0
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts2)
          (planning_entries :file_path ,testing-filepath
                            :headline_offset 1
                            :planning_type closed
                            :timestamp_offset 75)
          (headline_closures :file_path ,testing-filepath
                             :headline_offset 1
                             :parent_offset 1
                             :depth 0)
          (headlines :file_path ,testing-filepath
                     :headline_offset 1
                     :headline_text "headline"
                     :keyword nil
                     :effort nil
                     :priority nil
                     :is_archived 0
                     :is_commented 0
                     :content nil)))))

  ;; tags table

  (it "single tag"
    (expect-sql-tbls (headline_tags) "* headline :sometag:"
      `((headline_tags :file_path ,testing-filepath
                       :headline_offset 1
                       :tag "sometag"
                       :is_inherited 0))))

  (it "multiple tags"
    (expect-sql-tbls (headline_tags) (list "* headline :onetag:"
                                           "* headline :twotag:")
      `((headline_tags :file_path ,testing-filepath
                       :headline_offset 21
                       :tag "twotag"
                       :is_inherited 0)
        (headline_tags :file_path ,testing-filepath
                       :headline_offset 1
                       :tag "onetag"
                       :is_inherited 0))))

  (it "single tag (child headline)"
    (setq org-sql-use-tag-inheritance t)
    (expect-sql-tbls (headline_tags) (list "* parent :onetag:"
                                           "** nested")
      `((headline_tags :file_path ,testing-filepath
                       :headline_offset 1
                       :tag "onetag"
                       :is_inherited 0))))

  (it "inherited tag (ARCHIVE_ITAGS)"
    ;; TODO clean up the variable settings elsewhere
    (expect-sql-tbls (headline_tags) (list "* parent"
                                           ":PROPERTIES:"
                                           ":ARCHIVE_ITAGS: sometag"
                                           ":END:")
      `((headline_tags :file_path ,testing-filepath
                       :headline_offset 1
                       :tag "sometag"
                       :is_inherited 1))))

  (it "inherited tag (option off)"
    ;; TODO clean up the variable settings elsewhere
    (setq org-sql-use-tag-inheritance nil)
    (expect-sql-tbls (headline_tags) (list "* parent :onetag:"
                                           "** nested")
      `((headline_tags :file_path ,testing-filepath
                       :headline_offset 1
                       :tag "onetag"
                       :is_inherited 0))))
  
  (it "single file tag"
    (expect-sql-tbls (file_tags) (list "#+FILETAGS: foo"
                                       "* headline")
      `((file_tags :file_path ,testing-filepath
                   :tag "foo"))))

  (it "multiple file tags"
    (expect-sql-tbls (file_tags) (list "#+FILETAGS: foo bar"
                                       "#+FILETAGS: bang"
                                       "#+FILETAGS: bar"
                                       "* headline")
      `((file_tags :file_path ,testing-filepath
                   :tag "bang")
        (file_tags :file_path ,testing-filepath
                   :tag "bar")
        (file_tags :file_path ,testing-filepath
                   :tag "foo"))))

  ;;  ;; timestamp table

  (it "closed timestamp"
    (let* ((ts "<2112-01-01 Thu>")
           (planning (format "CLOSED: %s" ts)))
      (expect-sql-tbls (timestamps) (list "* parent"
                                          planning)
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 18
                      :is_active 1
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts)
                      :start_is_long 0
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts)))))

  (it "closed timestamp (long)"
    (let* ((ts "<2112-01-01 Thu 00:00>")
           (planning (format "CLOSED: %s" ts)))
      (expect-sql-tbls (timestamps) (list "* parent"
                                          planning)
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 18
                      :is_active 1
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts)
                      :start_is_long 1
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts)))))

  (it "timestamp deadline (repeater)"
    (let* ((ts "<2112-01-01 Thu +2d>")
           (planning (format "DEADLINE: %s" ts)))
      (expect-sql-tbls (timestamps) (list "* parent"
                                          planning)
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 20
                      :is_active 1
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type cumulate
                      :repeat_value 2
                      :repeat_unit day
                      :time_start ,(org-ts-to-unixtime ts)
                      :start_is_long 0
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts)))))

  (it "timestamp deadline (warning)"
    (let* ((ts "<2112-01-01 Thu -2d>")
           (planning (format "DEADLINE: %s" ts)))
      (expect-sql-tbls (timestamps) (list "* parent"
                                          planning)
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 20
                      :is_active 1
                      :warning_type all
                      :warning_value 2
                      :warning_unit day
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts)
                      :start_is_long 0
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts)))))

  ;; TODO obviously this is bullshit
  (it "timestamp deadline (ranged)"
    (let* ((ts0 "<2112-01-01 Thu>")
           (ts1 "<2112-01-02 Fri>")
           (ts (format "%s--%s" ts0 ts1))
           (planning (format "DEADLINE: %s" ts)))
      (expect-sql-tbls (timestamps) (list "* parent"
                                          planning)
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 20
                      :is_active 1
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts0)
                      :start_is_long 0
                      :time_end ,(org-ts-to-unixtime ts1)
                      :end_is_long 0
                      :raw_value ,ts)))))

  ;; link table

  (it "single link"
    (expect-sql-tbls (links) (list "* parent"
                                   "https://example.com")
      `((links :file_path ,testing-filepath
               :headline_offset 1
               :link_offset 10
               :link_path "//example.com"
               :link_text ""
               :link_type "https"))))

  (it "two links"
    (expect-sql-tbls (links) (list "* parent"
                                   "https://example.org"
                                   "https://example.com")
      `((links :file_path ,testing-filepath
               :headline_offset 1
               :link_offset 30
               :link_path "//example.com"
               :link_text ""
               :link_type "https")
        (links :file_path ,testing-filepath
               :headline_offset 1
               :link_offset 10
               :link_path "//example.org"
               :link_text ""
               :link_type "https"))))
  
  (it "link with description"
    (expect-sql-tbls (links) (list "* parent"
                                   "[[https://example.org][relevant]]")
      `((links :file_path ,testing-filepath
               :headline_offset 1
               :link_offset 10
               :link_path "//example.org"
               :link_text "relevant"
               :link_type "https"))))

  (it "file link"
    (expect-sql-tbls (links) (list "* parent"
                                   "file:///tmp/eternalblue.exe")
      `((links :file_path ,testing-filepath
               :headline_offset 1
               :link_offset 10
               :link_path "/tmp/eternalblue.exe"
               :link_text ""
               :link_type "file"))))

  (it "single link (ignored)"
    (let ((org-sql-ignored-link-types 'all))
      (expect-sql-tbls (links) (list "* parent"
                                     "file:///tmp/eternalblue.exe")
        nil)))

  ;; property table

  (it "single property"
    (expect-sql-tbls (properties headline_properties)
        (list "* parent"
              ":PROPERTIES:"
              ":key: val"
              ":END:")
      `((headline_properties :file_path ,testing-filepath
                             :headline_offset 1
                             :property_offset 23)
        (properties :file_path ,testing-filepath
                    :property_offset 23
                    :key_text "key"
                    :val_text "val"))))

  (it "multiple properties"
    (expect-sql-tbls (properties headline_properties)
        (list "* parent"
              ":PROPERTIES:"
              ":p1: ragtime dandies"
              ":p2: this time its personal"
              ":END:")
      `((headline_properties :file_path ,testing-filepath
                             :headline_offset 1
                             :property_offset 44)
        (properties :file_path ,testing-filepath
                    :property_offset 44
                    :key_text "p2"
                    :val_text "this time its personal")
        (headline_properties :file_path ,testing-filepath
                             :headline_offset 1
                             :property_offset 23)
        (properties :file_path ,testing-filepath
                    :property_offset 23
                    :key_text "p1"
                    :val_text "ragtime dandies"))))

  (it "single file property"
    (expect-sql-tbls (properties file_properties)
        (list "#+PROPERTY: FOO bar"
              "* parent")
      `((file_properties :file_path ,testing-filepath
                         :property_offset 1)
        (properties :file_path ,testing-filepath
                    :property_offset 1
                    :key_text "FOO"
                    :val_text "bar"))))

  ;; ;; TODO add inherited properties once they exist

  (it "single clock (closed)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts0 "[2112-01-01 Fri 00:00]")
           (ts1 "[2112-01-02 Sat 01:00]")
           (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
      (expect-sql-tbls (clocks) (list "* parent"
                                      ":LOGBOOK:"
                                      clock
                                      ":END:")
        ;; TODO what happens if we join tables and names collide?
        `((clocks :file_path ,testing-filepath
                  :headline_offset 1
                  :clock_offset 20
                  :time_start ,(org-ts-to-unixtime ts0)
                  :time_end ,(org-ts-to-unixtime ts1)
                  :clock_note nil)))))

  (it "single clock (open)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts "[2112-01-01 Fri 00:00]")
           (clock (format "CLOCK: %s" ts)))
      (expect-sql-tbls (clocks) (list "* parent"
                                      ":LOGBOOK:"
                                      clock
                                      ":END:")
        `((clocks :file_path ,testing-filepath
                  :headline_offset 1
                  :clock_offset 20
                  :time_start ,(org-ts-to-unixtime ts)
                  :time_end nil
                  :clock_note nil)))))

  (it "multiple clocks"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts0 "[2112-01-01 Fri 00:00]")
           (ts1 "[2112-01-01 Fri 01:00]")
           (clock0 (format "CLOCK: %s" ts0))
           (clock1 (format "CLOCK: %s" ts1)))
      (expect-sql-tbls (clocks) (list "* parent"
                                      ":LOGBOOK:"
                                      clock0
                                      clock1
                                      ":END:")
        `((clocks :file_path ,testing-filepath
                  :headline_offset 1
                  :clock_offset 50
                  :time_start ,(org-ts-to-unixtime ts1)
                  :time_end nil
                  :clock_note nil)
          (clocks :file_path ,testing-filepath
                  :headline_offset 1
                  :clock_offset 20
                  :time_start ,(org-ts-to-unixtime ts0)
                  :time_end nil
                  :clock_note nil)))))

  (it "logbook item (note)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts "[2112-01-01 Fri 00:00]")
           (header (format "Note taken on %s" ts))
           (note "fancy note"))
      (expect-sql-tbls (logbook_entries) (list "* parent"
                                               ":LOGBOOK:"
                                               (format "- %s \\\\" header)
                                               (format "  %s" note)
                                               ":END:")
        `((logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type note
                           :time_logged ,(org-ts-to-unixtime ts)
                           :header ,header
                           :note ,note)))))

  (it "logbook item (state change)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts "[2112-01-01 Fri 00:00]")
           (header (format "State \"DONE\"       from \"TODO\"       %s" ts)))
      (expect-sql-tbls (logbook_entries state_changes)
          (list "* parent"
                ":LOGBOOK:"
                (format "- %s" header)
                ":END:")
        `((state_changes :file_path ,testing-filepath
                         :entry_offset 20
                         :state_old "TODO"
                         :state_new "DONE")
          (logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type state
                           :time_logged ,(org-ts-to-unixtime ts)
                           :header ,header
                           :note nil)))))

  (it "logbook item (reschedule)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts0 "[2112-01-01 Fri 00:00]")
           (ts1 "[2112-01-01 Fri 01:00]")
           (header (format "Rescheduled from \"%s\" on %s" ts0 ts1)))
      (expect-sql-tbls (logbook_entries timestamps planning_changes)
          (list "* parent"
                ":LOGBOOK:"
                (format "- %s" header)
                ":END:")
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 40
                      :is_active 0
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts0)
                      :start_is_long 1
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts0)
          (planning_changes :file_path ,testing-filepath
                            :entry_offset 20
                            :timestamp_offset 40)
          (logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type reschedule
                           :time_logged ,(org-ts-to-unixtime ts1)
                           :header ,header
                           :note nil)))))

  (it "logbook item (redeadline)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts0 "[2112-01-01 Fri 00:00]")
           (ts1 "[2112-01-01 Fri 01:00]")
           (header (format "New deadline from \"%s\" on %s" ts0 ts1)))
      (expect-sql-tbls (logbook_entries timestamps planning_changes)
          (list "* parent"
                ":LOGBOOK:"
                (format "- %s" header)
                ":END:")
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 41
                      :is_active 0
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts0)
                      :start_is_long 1
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts0)
          (planning_changes :file_path ,testing-filepath
                            :entry_offset 20
                            :timestamp_offset 41)
          (logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type redeadline
                           :time_logged ,(org-ts-to-unixtime ts1)
                           :header ,header
                           :note nil)))))

  (it "logbook item (delschedule)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts0 "[2112-01-01 Fri 00:00]")
           (ts1 "[2112-01-01 Fri 01:00]")
           (header (format "Not scheduled, was \"%s\" on %s" ts0 ts1)))
      (expect-sql-tbls (logbook_entries timestamps planning_changes)
          (list "* parent"
                ":LOGBOOK:"
                (format "- %s" header)
                ":END:")
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 42
                      :is_active 0
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts0)
                      :start_is_long 1
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts0)
          (planning_changes :file_path ,testing-filepath
                            :entry_offset 20
                            :timestamp_offset 42)
          (logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type delschedule
                           :time_logged ,(org-ts-to-unixtime ts1)
                           :header ,header
                           :note nil)))))

  (it "logbook item (deldeadline)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts0 "[2112-01-01 Fri 00:00]")
           (ts1 "[2112-01-01 Fri 01:00]")
           (header (format "Removed deadline, was \"%s\" on %s" ts0 ts1)))
      (expect-sql-tbls (logbook_entries timestamps planning_changes)
          (list "* parent"
                ":LOGBOOK:"
                (format "- %s" header)
                ":END:")
        `((timestamps :file_path ,testing-filepath
                      :headline_offset 1
                      :timestamp_offset 45
                      :is_active 0
                      :warning_type nil
                      :warning_value nil
                      :warning_unit nil
                      :repeat_type nil
                      :repeat_value nil
                      :repeat_unit nil
                      :time_start ,(org-ts-to-unixtime ts0)
                      :start_is_long 1
                      :time_end nil
                      :end_is_long nil
                      :raw_value ,ts0)
          (planning_changes :file_path ,testing-filepath
                            :entry_offset 20
                            :timestamp_offset 45)
          (logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type deldeadline
                           :time_logged ,(org-ts-to-unixtime ts1)
                           :header ,header
                           :note nil)))))

  (it "logbook item (refile)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts "[2112-01-01 Fri 00:00]")
           (header (format "Refiled on %s" ts)))
      (expect-sql-tbls (logbook_entries) (list "* parent"
                                               ":LOGBOOK:"
                                               (format "- %s" header)
                                               ":END:")
        `((logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type refile
                           :time_logged ,(org-ts-to-unixtime ts)
                           :header ,header
                           :note nil)))))

  (it "logbook item (done)"
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts "[2112-01-01 Fri 00:00]")
           (header (format "CLOSING NOTE %s" ts)))
      (expect-sql-tbls (logbook_entries) (list "* parent"
                                               ":LOGBOOK:"
                                               (format "- %s" header)
                                               ":END:")
        `((logbook_entries :file_path ,testing-filepath
                           :headline_offset 1
                           :entry_offset 20
                           :entry_type done
                           :time_logged ,(org-ts-to-unixtime ts)
                           :header ,header
                           :note nil))))))

;;; org-sql-test-internal.el ends here
