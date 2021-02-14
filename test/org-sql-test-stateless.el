;;; org-sql-test-stateless.el --- Stateless tests for org-sql -*- lexical-binding: t; -*-

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

;; This spec tests the stateless functions in org-sql

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

;; this is just the output from some random file (doesn't matter which one)
(defconst testing-attributes (list nil 1 1000 1000
                                   '(21670 15994 677140 892000)
                                   '(21670 15994 677140 892000)
                                   '(22909 25484 42946 839000)
                                   0 "-rw-r--r--" t 435278 65025))

(defconst testing-hash "123456")

(defconst testing-size 2112)

(defconst testing-lines 666)

(defconst testing-file_metadata
  `(file_metadata (,testing-filepath
                   ,testing-hash
                   ,(file-attribute-user-id testing-attributes)
                   ,(file-attribute-group-id testing-attributes)
                   ,(->> (file-attribute-modification-time testing-attributes)
                         (float-time)
                         (round))
                   ,(->> (file-attribute-status-change-time testing-attributes)
                         (float-time)
                         (round))
                   ,(file-attribute-modes testing-attributes))))

(defconst testing-outlines
  `(outlines (,testing-hash ,testing-size ,testing-lines)))

(defconst init-ids
  (list :headline-id 1
        :timestamp-id 1
        :entry-id 1
        :link-id 1
        :property-id 1
        :clock-id 1))

(defmacro expect-sql* (in tbl res-form)
  `(progn
     (insert (list-to-lines ,in))
     (let ((res ,res-form))
       (expect res :to-equal ,tbl))))

(defun buffer-get-sml ()
  (let ((lb-config (list :log-into-drawer org-log-into-drawer
                         :clock-into-drawer org-clock-into-drawer
                         :clock-out-notes org-log-note-clock-out))
        (paths-with-attributes (list (cons testing-filepath testing-attributes)))
        (acc (org-sql--init-acc (-clone init-ids))))
    (--> (org-ml-parse-this-buffer)
      (org-sql--to-outline-config testing-hash paths-with-attributes
                                  org-log-note-headings '("TODO" "DONE")
                                  lb-config testing-size testing-lines it)
      (org-sql--outline-config-to-insert-alist acc it)
      (plist-get it :inserts)
      (-filter #'cdr it))))

(defmacro expect-sql (in tbl)
  (declare (indent 1))
  `(expect-sql* ,in ,tbl (buffer-get-sml)))

(defmacro expect-sql-tbls (names in tbl)
  (declare (indent 2))
  `(expect-sql* ,in ,tbl (->> (buffer-get-sml)
                              (--filter (member (car it) ',names)))))

(defmacro expect-sql-tbls-multi (names in &rest forms)
  (declare (indent 2))
  (unless (= 0 (mod (length forms) 3))
    (error "Missing form"))
  (let ((specs
         (->> (-partition 3 forms)
              (--map (-let (((title let-forms tbl) it))
                       `(it ,title
                          (let (,@let-forms)
                            (expect-sql-tbls ,names ,in ,tbl))))))))
    `(progn ,@specs)))

(defmacro expect-sql-logbook-item (in log-note-headings entry)
  (declare (indent 2))
  `(progn
     (insert (list-to-lines ,in))
     (cl-flet
         ((props-to-string
           (props e)
           (->> (cdr e)
                (-partition 2)
                (--map-when (memq (car it) props)
                            (list (car it)
                                  (-some-> (cadr it)
                                    (org-ml-to-trimmed-string))))
                (apply #'append)
                (cons (car e)))))
       (let* ((item (org-ml-parse-item-at 1))
              (headline (->> (org-ml-build-headline! :title-text "dummy")
                             ;; TODO shouldn't this be settable?
                             (org-ml--set-property-nocheck :begin 1)))
              (lb-config (list :log-into-drawer org-log-into-drawer
                               :clock-into-drawer org-clock-into-drawer
                               :clock-out-notes org-log-note-clock-out))
              (paths-with-attributes
               (list (cons testing-filepath testing-attributes)))
              (outline-config (org-sql--to-outline-config testing-hash paths-with-attributes
                                                       ,log-note-headings '("TODO" "DONE")
                                                       lb-config testing-size testing-lines
                                                       nil))
              (hstate (org-sql--to-hstate 1 outline-config headline))
              (entry (->> (org-sql--item-to-entry hstate item)
                          (props-to-string (list :ts
                                                 :ts-active
                                                 :short-ts
                                                 :short-ts-active
                                                 :old-ts
                                                 :new-ts)))))
         (should (equal entry ,entry))))))

(describe "logbook entry spec"
  ;; don't test clocks here since they are way simpler
  (before-all
    (org-mode))

  (before-each
    (erase-buffer))

  (it "default - none"
    (expect-sql-logbook-item (list "- logbook item \\\\"
                                   "  fancy note")
                             org-log-note-headings
                             `(none :outline-hash ,testing-hash
                                    :header-text "logbook item"
                                    :note-text "fancy note"
                                    :user nil
                                    :user-full nil
                                    :ts nil
                                    :ts-active nil
                                    :short-ts nil
                                    :short-ts-active nil
                                    :old-ts nil
                                    :new-ts nil
                                    :old-state nil
                                    :new-state nil)))

  (it "default - state"
    (let* ((ts "[2112-01-03 Sun]")
           (h (format "State \"DONE\" from \"TODO\" %s" ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(state :outline-hash ,testing-hash
                                       :header-text ,h
                                       :note-text "fancy note"
                                       :user nil
                                       :user-full nil
                                       :ts ,ts
                                       :ts-active nil
                                       :short-ts nil
                                       :short-ts-active nil
                                       :old-ts nil
                                       :new-ts nil
                                       :old-state "TODO"
                                       :new-state "DONE"))))

  (it "default - refile"
    (let* ((ts "[2112-01-03 Sun]")
           (h (format "Refiled on %s" ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(refile :outline-hash ,testing-hash
                                        :header-text ,h
                                        :note-text "fancy note"
                                        :user nil
                                        :user-full nil
                                        :ts ,ts
                                        :ts-active nil
                                        :short-ts nil
                                        :short-ts-active nil
                                        :old-ts nil
                                        :new-ts nil
                                        :old-state nil
                                        :new-state nil))))

  (it "default - note"
    (let* ((ts "[2112-01-03 Sun]")
           (h (format "Note taken on %s" ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(note :outline-hash ,testing-hash
                                      :header-text ,h
                                      :note-text "fancy note"
                                      :user nil
                                      :user-full nil
                                      :ts ,ts
                                      :ts-active nil
                                      :short-ts nil
                                      :short-ts-active nil
                                      :old-ts nil
                                      :new-ts nil
                                      :old-state nil
                                      :new-state nil))))

  (it "default - done"
    (let* ((ts "[2112-01-03 Sun]")
           (h (format "CLOSING NOTE %s" ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(done :outline-hash ,testing-hash
                                      :header-text ,h
                                      :note-text "fancy note"
                                      :user nil
                                      :user-full nil
                                      :ts ,ts
                                      :ts-active nil
                                      :short-ts nil
                                      :short-ts-active nil
                                      :old-ts nil
                                      :new-ts nil
                                      :old-state nil
                                      :new-state nil))))

  (it "default - reschedule"
    (let* ((ts "[2112-01-03 Sun]")
           (ts0 "[2112-01-04 Mon]")
           (h (format "Rescheduled from \"%s\" on %s" ts0 ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(reschedule :outline-hash ,testing-hash
                                            :header-text ,h
                                            :note-text "fancy note"
                                            :user nil
                                            :user-full nil
                                            :ts ,ts
                                            :ts-active nil
                                            :short-ts nil
                                            :short-ts-active nil
                                            :old-ts ,ts0
                                            :new-ts nil
                                            :old-state nil
                                            :new-state nil))))

  (it "default - delschedule"
    (let* ((ts "[2112-01-03 Sun]")
           (ts0 "[2112-01-04 Mon]")
           (h (format "Not scheduled, was \"%s\" on %s" ts0 ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(delschedule :outline-hash ,testing-hash
                                             :header-text ,h
                                             :note-text "fancy note"
                                             :user nil
                                             :user-full nil
                                             :ts ,ts
                                             :ts-active nil
                                             :short-ts nil
                                             :short-ts-active nil
                                             :old-ts ,ts0
                                             :new-ts nil
                                             :old-state nil
                                             :new-state nil))))

  (it "default - redeadline"
    (let* ((ts "[2112-01-03 Sun]")
           (ts0 "[2112-01-04 Mon]")
           (h (format "New deadline from \"%s\" on %s" ts0 ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(redeadline :outline-hash ,testing-hash
                                            :header-text ,h
                                            :note-text "fancy note"
                                            :user nil
                                            :user-full nil
                                            :ts ,ts
                                            :ts-active nil
                                            :short-ts nil
                                            :short-ts-active nil
                                            :old-ts ,ts0
                                            :new-ts nil
                                            :old-state nil
                                            :new-state nil))))

  (it "default - deldeadline"
    (let* ((ts "[2112-01-03 Sun]")
           (ts0 "[2112-01-04 Mon]")
           (h (format "Removed deadline, was \"%s\" on %s" ts0 ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               org-log-note-headings
                               `(deldeadline :outline-hash ,testing-hash
                                             :header-text ,h
                                             :note-text "fancy note"
                                             :user nil
                                             :user-full nil
                                             :ts ,ts
                                             :ts-active nil
                                             :short-ts nil
                                             :short-ts-active nil
                                             :old-ts ,ts0
                                             :new-ts nil
                                             :old-state nil
                                             :new-state nil))))

  (it "custom - user"
    (let* ((user "eddie666")
           (h (format "User %s is the best user" user)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               '((user . "User %u is the best user"))
                               `(user :outline-hash ,testing-hash
                                      :header-text ,h
                                      :note-text "fancy note"
                                      :user ,user
                                      :user-full nil
                                      :ts nil
                                      :ts-active nil
                                      :short-ts nil
                                      :short-ts-active nil
                                      :old-ts nil
                                      :new-ts nil
                                      :old-state nil
                                      :new-state nil))))

  (it "custom - user full"
    ;; TODO this variable can have spaces and such, which will currently not
    ;; be matched
    (let* ((userfull "FullName")
           (h (format "User %s is the best user" userfull)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               '((userfull . "User %u is the best user"))
                               `(userfull :outline-hash ,testing-hash
                                          :header-text ,h
                                          :note-text "fancy note"
                                          :user ,userfull
                                          :user-full nil
                                          :ts nil
                                          :ts-active nil
                                          :short-ts nil
                                          :short-ts-active nil
                                          :old-ts nil
                                          :new-ts nil
                                          :old-state nil
                                          :new-state nil))))

  (it "custom - active timestamp"
    (let* ((ts "<2112-01-01 Fri 00:00>")
           (h (format "I'm active now: %s" ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               '((activets . "I'm active now: %T"))
                               `(activets :outline-hash ,testing-hash
                                          :header-text ,h
                                          :note-text "fancy note"
                                          :user nil
                                          :user-full nil
                                          :ts nil
                                          :ts-active ,ts
                                          :short-ts nil
                                          :short-ts-active nil
                                          :old-ts nil
                                          :new-ts nil
                                          :old-state nil
                                          :new-state nil))))

  (it "custom - short timestamp"
    (let* ((ts "[2112-01-01 Fri]")
           (h (format "Life feels short now: %s" ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               '((shortts . "Life feels short now: %d"))
                               `(shortts :outline-hash ,testing-hash
                                         :header-text ,h
                                         :note-text "fancy note"
                                         :user nil
                                         :user-full nil
                                         :ts nil
                                         :ts-active nil
                                         :short-ts ,ts
                                         :short-ts-active nil
                                         :old-ts nil
                                         :new-ts nil
                                         :old-state nil
                                         :new-state nil))))

  (it "custom - short active timestamp"
    (let* ((ts "<2112-01-01 Fri>")
           (h (format "Life feels short now: %s" ts)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               '((shortts . "Life feels short now: %D"))
                               `(shortts :outline-hash ,testing-hash
                                         :header-text ,h
                                         :note-text "fancy note"
                                         :user nil
                                         :user-full nil
                                         :ts nil
                                         :ts-active nil
                                         :short-ts nil
                                         :short-ts-active ,ts
                                         :old-ts nil
                                         :new-ts nil
                                         :old-state nil
                                         :new-state nil))))

  (it "custom - old/new timestamps"
    (let* ((ts0 "[2112-01-01 Fri]")
           (ts1 "[2112-01-02 Sat]")
           (h (format "Fake clock: \"%s\"--\"%s\"" ts0 ts1)))
      (expect-sql-logbook-item (list (format "- %s \\\\" h) "  fancy note")
                               '((fakeclock . "Fake clock: %S--%s"))
                               `(fakeclock :outline-hash ,testing-hash
                                           :header-text ,h
                                           :note-text "fancy note"
                                           :user nil
                                           :user-full nil
                                           :ts nil
                                           :ts-active nil
                                           :short-ts nil
                                           :short-ts-active nil
                                           :old-ts ,ts0
                                           :new-ts ,ts1
                                           :old-state nil
                                           :new-state nil)))))

(describe "bulk insert spec"
  (before-all
    (org-mode))

  (before-each
    (erase-buffer))

  (describe "headlines"
    (it "single"
      (expect-sql "* headline"
                  `(,testing-outlines
                    ,testing-file_metadata
                    (headlines (1 ,testing-hash "headline" nil nil nil nil nil 0 0 nil))
                    (headline_closures (1 1 0)))))

    (it "two"
      (expect-sql (list "* headline"
                        "* another headline")
                  `(,testing-outlines
                    ,testing-file_metadata
                    (headlines (2 ,testing-hash "another headline" nil nil nil nil nil 0 0 nil)
                               (1 ,testing-hash "headline" nil nil nil nil nil 0 0 nil))
                    (headline_closures (2 2 0)
                                       (1 1 0)))))

    (it "fancy"
      (expect-sql (list "* TODO [#A] COMMENT another headline [1/2]"
                        ":PROPERTIES:"
                        ":Effort: 0:30"
                        ":END:"
                        "this /should/ appear")
                  `(,testing-outlines
                    ,testing-file_metadata
                    (headlines
                     (1 ,testing-hash "another headline [1/2]" "TODO" 30 "A" fraction 0.5
                        0 1 "this /should/ appear\n"))
                    (headline_closures
                     (1 1 0)))))

    (expect-sql-tbls-multi (outlines file_metadata headlines headline_closures)
                           (list "* headline"
                                 "** nested headline")
                           "nested (predicate applied to parent)"
                           ((org-sql-exclude-headline-predicate
                             (lambda (h)
                               (= 1 (org-ml-get-property :level h)))))
                           `(,testing-outlines
                             ,testing-file_metadata)

                           "nested (predicate applied to child)"
                           ((org-sql-exclude-headline-predicate
                             (lambda (h)
                               (= 2 (org-ml-get-property :level h)))))
                           `(,testing-outlines
                             ,testing-file_metadata
                             (headlines (1 ,testing-hash "headline" nil nil nil nil nil 0 0 nil))
                             (headline_closures (1 1 0)))
                           
                           "nested (no predicate)"
                           nil
                           `(,testing-outlines
                             ,testing-file_metadata
                             (headlines (2 ,testing-hash "nested headline" nil nil nil nil nil 0 0 nil)
                                        (1 ,testing-hash "headline" nil nil nil nil nil 0 0 nil))
                             (headline_closures (2 2 0)
                                                (2 1 1)
                                                (1 1 0))))

    (it "archived"
      (expect-sql "* headline :ARCHIVE:"
                  `(,testing-outlines
                    ,testing-file_metadata
                    (headlines (1 ,testing-hash "headline" nil nil nil nil nil 1 0 nil))
                    (headline_closures (1 1 0))))))

  (describe "planning entries"
    (let ((ts0 "<2112-01-01 Thu>")
          (ts1 "<2112-01-02 Fri>")
          (ts2 "[2112-01-03 Sat]"))
      (expect-sql-tbls-multi (planning_entries timestamps)
                             (list "* headline"
                                   (format "SCHEDULED: %s DEADLINE: %s CLOSED: %s" ts0 ts1 ts2))
                             "multiple (included)"
                             nil
                             `((timestamps (3 1 ,ts0 1 ,(org-ts-to-unixtime ts0) nil 0 nil)
                                           (2 1 ,ts1 1 ,(org-ts-to-unixtime ts1) nil 0 nil)
                                           (1 1 ,ts2 0 ,(org-ts-to-unixtime ts2) nil 0 nil))
                               (planning_entries (3 scheduled)
                                                 (2 deadline)
                                                 (1 closed)))

                             "multiple (exclude some)"
                             ((org-sql-excluded-headline-planning-types '(:closed)))
                             `((timestamps (2 1  ,ts0 1 ,(org-ts-to-unixtime ts0) nil 0 nil)
                                           (1 1 ,ts1 1 ,(org-ts-to-unixtime ts1) nil 0 nil))
                               (planning_entries (2 scheduled)
                                                 (1 deadline)))

                             "multiple (exclude all)"
                             ((org-sql-excluded-headline-planning-types '(:closed :scheduled :deadline)))
                             nil)))

  (describe "tags"
    (expect-sql-tbls-multi (headline_tags) (list "* headline :onetag:"
                                                 "* headline :twotag:")
                           "multiple (included)"
                           nil
                           `((headline_tags (2 "twotag" 0)
                                            (1 "onetag" 0)))

                           "multiple (exclude one)"
                           ((org-sql-excluded-tags '("onetag")))
                           `((headline_tags (2 "twotag" 0)))

                           "multiple (exclude all)"
                           ((org-sql-excluded-tags 'all))
                           nil)

    (it "single (child headline)"
      (setq org-sql-use-tag-inheritance t)
      (expect-sql-tbls (headline_tags) (list "* parent :onetag:"
                                             "** nested")
                       `((headline_tags (1 "onetag" 0)))))

    (expect-sql-tbls-multi (headline_tags) (list "* parent"
                                                 ":PROPERTIES:"
                                                 ":ARCHIVE_ITAGS: sometag"
                                                 ":END:")
                           "inherited (included)"
                           nil
                           `((headline_tags (1 "sometag" 1)))

                           "inherited (excluded)"
                           ((org-sql-exclude-inherited-tags t))
                           nil))

  (describe "file tags"
    (it "single"
      (expect-sql-tbls (file_tags) (list "#+FILETAGS: foo"
                                         "* headline")
                       `((file_tags (,testing-hash "foo")))))

    (it "multiple"
      (expect-sql-tbls (file_tags) (list "#+FILETAGS: foo bar"
                                         "#+FILETAGS: bang"
                                         "#+FILETAGS: bar"
                                         "* headline")
                       `((file_tags (,testing-hash "bang")
                                    (,testing-hash "bar")
                                    (,testing-hash "foo"))))))

  (describe "timestamp"
    (it "closed"
      (let* ((ts "<2112-01-01 Thu>")
             (planning (format "CLOSED: %s" ts)))
        (expect-sql-tbls (timestamps) (list "* parent"
                                            planning)
                         `((timestamps (1 1 ,ts 1 ,(org-ts-to-unixtime ts) nil 0 nil))))))

    (it "closed (long)"
      (let* ((ts "<2112-01-01 Thu 00:00>")
             (planning (format "CLOSED: %s" ts)))
        (expect-sql-tbls (timestamps) (list "* parent"
                                            planning)
                         `((timestamps (1 1 ,ts 1 ,(org-ts-to-unixtime ts) nil 1 nil))))))

    (it "deadline (repeater)"
      (let* ((ts "<2112-01-01 Thu +2d>")
             (planning (format "DEADLINE: %s" ts)))
        (expect-sql-tbls (timestamps timestamp_modifiers timestamp_repeaters)
                         (list "* parent"
                               planning)
                         `((timestamps (1 1 ,ts 1 ,(org-ts-to-unixtime ts) nil 0 nil))
                           (timestamp_repeaters (1 2 day cumulate))))))

    (it "deadline (warning)"
      (let* ((ts "<2112-01-01 Thu -2d>")
             (planning (format "DEADLINE: %s" ts)))
        (expect-sql-tbls (timestamps timestamp_modifiers timestamp_warnings)
                         (list "* parent"
                               planning)
                         `((timestamps (1 1 ,ts 1 ,(org-ts-to-unixtime ts) nil 0 nil))
                           (timestamp_warnings (1 2 day all))))))

    (let* ((ts1 "<2112-01-01 Thu>")
           (ts2 "[2112-01-02 Fri]"))
      (expect-sql-tbls-multi (timestamps) (list "* parent"
                                                ts1
                                                ts2)
                             "multiple content (included)"
                             nil
                             `((timestamps (2 1 ,ts2 0 ,(org-ts-to-unixtime ts2) nil 0 nil)
                                           (1 1 ,ts1 1 ,(org-ts-to-unixtime ts1) nil 0 nil)))

                             "multiple content (exclude some)"
                             ((org-sql-excluded-contents-timestamp-types '(inactive)))
                             `((timestamps (1 1 ,ts1 1 ,(org-ts-to-unixtime ts1) nil 0 nil)))

                             "multiple content (exclude all)"
                             ((org-sql-excluded-contents-timestamp-types 'all))
                             nil))

    (it "content (nested)"
      (let* ((ts "<2112-01-01 Thu>"))
        (expect-sql-tbls (timestamps) (list "* parent"
                                            "** child"
                                            ts)
                         `((timestamps (1 2 ,ts 1 ,(org-ts-to-unixtime ts) nil 0 nil))))))
    
    (it "content (ranged)"
      (let* ((ts0 "<2112-01-01 Thu>")
             (ts1 "<2112-01-02 Fri>")
             (ts (format "%s--%s" ts0 ts1)))
        (expect-sql-tbls (timestamps) (list "* parent"
                                            ts)
                         `((timestamps (1 1 ,ts 1 ,(org-ts-to-unixtime ts0)
                                          ,(org-ts-to-unixtime ts1) 0 0)))))))

  (describe "links"
    (expect-sql-tbls-multi (links) (list "* parent"
                                         "https://example.org"
                                         "file:///the/glass/prison")
                           "multiple (included)"
                           nil
                           `((links (2 1 "/the/glass/prison" "" "file")
                                    (1 1 "//example.org" "" "https")))

                           "multiple (exclude some)"
                           ((org-sql-excluded-link-types '("file")))
                           `((links (1 1 "//example.org" "" "https")))

                           "multiple (exclude all)"
                           ((org-sql-excluded-link-types 'all))
                           nil)

    (it "single (nested)"
      (expect-sql-tbls (links) (list "* parent"
                                     "** child"
                                     "https://example.com")
                       `((links (1 2 "//example.com" "" "https")))))
    
    (it "with description"
      (expect-sql-tbls (links) (list "* parent"
                                     "[[https://example.org][relevant]]")
                       `((links (1 1 "//example.org" "relevant" "https"))))))

  (describe "properties"
    (it "single"
      (expect-sql-tbls (properties headline_properties)
                       (list "* parent"
                             ":PROPERTIES:"
                             ":key: val"
                             ":END:")
                       `((properties (,testing-hash 1 "key" "val"))
                         (headline_properties (1 1)))))

    (it "multiple"
      (expect-sql-tbls (properties headline_properties)
                       (list "* parent"
                             ":PROPERTIES:"
                             ":p1: ragtime dandies"
                             ":p2: this time its personal"
                             ":END:")
                       `((properties (,testing-hash 2 "p2" "this time its personal")
                                     (,testing-hash 1 "p1" "ragtime dandies"))
                         (headline_properties (1 2)
                                              (1 1)))))

    ;; TODO add inherited properties once they exist

    (it "single file"
      (expect-sql-tbls (properties file_properties)
                       (list "#+PROPERTY: FOO bar"
                             "* parent")
                       `((properties (,testing-hash 1 "FOO" "bar"))))))

  (describe "logbook"
    (describe "clocks"
      (it "single (closed)"
        (let* ((ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks) (list "* parent"
                                          ":LOGBOOK:"
                                          clock
                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) nil))))))

      (it "single (open)"
        (let* ((ts "[2112-01-01 Fri 00:00]")
               (clock (format "CLOCK: %s" ts)))
          (expect-sql-tbls (clocks) (list "* parent"
                                          ":LOGBOOK:"
                                          clock
                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts) nil nil))))))

      (let* ((ts "[2112-01-01 Fri 00:00]")
             (clock (format "CLOCK: %s" ts)))
        (expect-sql-tbls-multi (clocks) (list "* parent"
                                              ":LOGBOOK:"
                                              clock
                                              "- random"
                                              ":END:")
                               "single (note - included)"
                               ((org-log-note-clock-out t))
                               `((clocks (1 1 ,(org-ts-to-unixtime ts) nil "random")))

                               "single (note - excluded)"
                               ((org-log-note-clock-out t)
                                (org-sql-exclude-clock-notes t))
                               `((clocks (1 1 ,(org-ts-to-unixtime ts) nil nil)))))

      (it "multiple"
        (let* ((ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-01 Fri 01:00]")
               (clock0 (format "CLOCK: %s" ts0))
               (clock1 (format "CLOCK: %s" ts1)))
          (expect-sql-tbls (clocks) (list "* parent"
                                          ":LOGBOOK:"
                                          clock0
                                          clock1
                                          ":END:")
                           `((clocks (2 1 ,(org-ts-to-unixtime ts1) nil nil)
                                     (1 1 ,(org-ts-to-unixtime ts0) nil nil)))))))

    (describe "items"
      (it "multiple"
        (let* ((ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-01 Fri 01:00]")
               (note0 (format "Note taken on %s" ts0))
               (note1 (format "Note taken on %s" ts1)))
          (expect-sql-tbls (logbook_entries) (list "* parent"
                                                   (format "- %s" note0)
                                                   (format "- %s" note1))
                           `((logbook_entries (2 1 "note" ,(org-ts-to-unixtime ts1) ,note1 nil)
                                              (1 1 "note" ,(org-ts-to-unixtime ts0) ,note0 nil))))))

      (let* ((ts "[2112-01-01 Fri 00:00]")
             (header (format "Note taken on %s" ts))
             (note "fancy note"))
        (expect-sql-tbls-multi (logbook_entries) (list "* parent"
                                                       (format "- %s \\\\" header)
                                                       (format "  %s" note))
                               "note (included)"
                               nil
                               `((logbook_entries (1 1 "note" ,(org-ts-to-unixtime ts) ,header ,note)))

                               "note (exclude)"
                               ((org-sql-excluded-logbook-types '(note)))
                               nil))

      (let* ((ts "[2112-01-01 Fri 00:00]")
             (header (format "State \"DONE\"       from \"TODO\"       %s" ts)))
        (expect-sql-tbls-multi (logbook_entries state_changes)
                               (list "* parent"
                                     (format "- %s" header))
                               "state change (included)"
                               nil
                               `((logbook_entries (1 1 "state" ,(org-ts-to-unixtime ts) ,header nil))
                                 (state_changes (1 "TODO" "DONE")))

                               "state change (excluded)"
                               ((org-sql-excluded-logbook-types '(state)))
                               nil))

      (let* ((ts0 "[2112-01-01 Fri 00:00]")
             (ts1 "[2112-01-01 Fri 01:00]")
             (header (format "Rescheduled from \"%s\" on %s" ts0 ts1)))
        (expect-sql-tbls-multi (logbook_entries timestamps planning_changes)
                               (list "* parent"
                                     (format "- %s" header))
                               "rescheduled (included)"
                               nil
                               `((timestamps (1 1 ,ts0 0 ,(org-ts-to-unixtime ts0) nil 1 nil))
                                 (logbook_entries (1 1 "reschedule" ,(org-ts-to-unixtime ts1) ,header nil))
                                 (planning_changes (1 1)))

                               "rescheduled (excluded)"
                               ((org-sql-excluded-logbook-types '(reschedule)))
                               nil))

      (let* ((ts0 "[2112-01-01 Fri 00:00]")
             (ts1 "[2112-01-01 Fri 01:00]")
             (header (format "New deadline from \"%s\" on %s" ts0 ts1)))
        (expect-sql-tbls-multi (logbook_entries timestamps planning_changes)
                               (list "* parent"
                                     (format "- %s" header))
                               "redeadline (included)"
                               nil
                               `((timestamps (1 1 ,ts0 0 ,(org-ts-to-unixtime ts0) nil 1 nil))
                                 (logbook_entries (1 1 "redeadline" ,(org-ts-to-unixtime ts1)
                                                     ,header nil))
                                 (planning_changes (1 1)))

                               "redeadline (excluded)"
                               ((org-sql-excluded-logbook-types '(redeadline)))
                               nil))

      (let* ((ts0 "[2112-01-01 Fri 00:00]")
             (ts1 "[2112-01-01 Fri 01:00]")
             (header (format "Not scheduled, was \"%s\" on %s" ts0 ts1)))
        (expect-sql-tbls-multi (logbook_entries timestamps planning_changes)
                               (list "* parent"
                                     (format "- %s" header))
                               "delschedule (included)"
                               nil
                               `((timestamps (1 1 ,ts0 0 ,(org-ts-to-unixtime ts0) nil 1 nil))
                                 (logbook_entries (1 1 "delschedule" ,(org-ts-to-unixtime ts1)
                                                     ,header nil))
                                 (planning_changes (1 1)))

                               "delschedule (excluded)"
                               ((org-sql-excluded-logbook-types '(delschedule)))
                               nil))

      (let* ((ts0 "[2112-01-01 Fri 00:00]")
             (ts1 "[2112-01-01 Fri 01:00]")
             (header (format "Removed deadline, was \"%s\" on %s" ts0 ts1)))
        (expect-sql-tbls-multi (logbook_entries timestamps planning_changes)
                               (list "* parent"
                                     (format "- %s" header))
                               "deldeadline (included)"
                               nil
                               `((timestamps (1 1 ,ts0 0 ,(org-ts-to-unixtime ts0) nil 1 nil))
                                 (logbook_entries (1 1 "deldeadline" ,(org-ts-to-unixtime ts1)
                                                     ,header nil))
                                 (planning_changes (1 1)))

                               "deldeadline (excluded)"
                               ((org-sql-excluded-logbook-types '(deldeadline)))
                               nil))

      (let* ((ts "[2112-01-01 Fri 00:00]")
             (header (format "Refiled on %s" ts)))
        (expect-sql-tbls-multi (logbook_entries) (list "* parent"
                                                       (format "- %s" header))
                               "refile (included)"
                               nil
                               `((logbook_entries (1 1 "refile" ,(org-ts-to-unixtime ts) ,header nil)))

                               "refile (excluded)"
                               ((org-sql-excluded-logbook-types '(refile)))
                               nil))

      (let* ((ts "[2112-01-01 Fri 00:00]")
             (header (format "CLOSING NOTE %s" ts)))
        (expect-sql-tbls-multi (logbook_entries) (list "* parent"
                                                       (format "- %s" header))
                               "done (included)"
                               nil
                               `((logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil)))

                               "done (excluded)"
                               ((org-sql-excluded-logbook-types '(done)))
                               nil)))

    (describe "mixture"
      (it "clock + non-note"
        (let* ((ts "[2112-01-01 Fri 00:00]")
               (header (format "CLOSING NOTE %s" ts))
               (ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks logbook_entries) (list "* parent"
                                                          ":LOGBOOK:"
                                                          clock
                                                          ":END:"
                                                          (format "- %s" header))
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) nil))
                             (logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil))))))

      (it "clock + note + non-note"
        (let* ((org-log-note-clock-out t)
               (ts "[2112-01-01 Fri 00:00]")
               (header (format "CLOSING NOTE %s" ts))
               (ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks logbook_entries) (list "* parent"
                                                          ":LOGBOOK:"
                                                          clock
                                                          " - this is a clock note"
                                                          ":END:"
                                                          (format "- %s" header))
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) "this is a clock note"))
                             (logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil))))))

      (it "non-note + clock"
        (let* ((ts "[2112-01-01 Fri 00:00]")
               (header (format "CLOSING NOTE %s" ts))
               (ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks logbook_entries) (list "* parent"
                                                          (format "- %s" header)
                                                          ":LOGBOOK:"
                                                          clock
                                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) nil))
                             (logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil)))))) 

      (it "non-note + clock + clock note"
        (let* ((org-log-note-clock-out t)
               (ts "[2112-01-01 Fri 00:00]")
               (header (format "CLOSING NOTE %s" ts))
               (ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks logbook_entries) (list "* parent"
                                                          (format "- %s" header)
                                                          ":LOGBOOK:"
                                                          clock
                                                          "- this is a clock note"
                                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) "this is a clock note"))
                             (logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil)))))))

    (describe "non-default drawer configs"
      (it "log drawer (global)"
        (let* ((org-log-into-drawer "LOGGING")
               (ts "[2112-01-01 Fri 00:00]")
               (header (format "CLOSING NOTE %s" ts)))
          (expect-sql-tbls (logbook_entries) (list "* parent"
                                                   ":LOGGING:"
                                                   (format "- %s" header)
                                                   ":END:")
                           `((logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil))))))

      (it "log drawer (file)"
        (let* ((ts "[2112-01-01 Fri 00:00]")
               (header (format "CLOSING NOTE %s" ts)))
          (expect-sql-tbls (logbook_entries) (list "#+STARTUP: logdrawer"
                                                   "* parent"
                                                   ":LOGBOOK:"
                                                   (format "- %s" header)
                                                   ":END:")
                           `((logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil))))))
      (it "log drawer (property)"
        (let* ((ts "[2112-01-01 Fri 00:00]")
               (header (format "CLOSING NOTE %s" ts)))
          (expect-sql-tbls (logbook_entries) (list "* parent"
                                                   ":PROPERTIES:"
                                                   ":LOG_INTO_DRAWER: LOGGING"
                                                   ":END:"
                                                   ":LOGGING:"
                                                   (format "- %s" header)
                                                   ":END:")
                           `((logbook_entries (1 1 "done" ,(org-ts-to-unixtime ts) ,header nil))))))

      (it "clock drawer (global)"
        (let* ((org-clock-into-drawer "CLOCKING")
               (ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks) (list "* parent"
                                          ":CLOCKING:"
                                          clock
                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) nil))))))

      (it "clock drawer (property)"
        (let* ((ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks) (list "* parent"
                                          ":PROPERTIES:"
                                          ":CLOCK_INTO_DRAWER: CLOCKING"
                                          ":END:"
                                          ":CLOCKING:"
                                          clock
                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) nil))))))

      (it "clock note (global)"
        (let* ((org-log-note-clock-out t)
               (ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks) (list "* parent"
                                          ":LOGBOOK:"
                                          clock
                                          "- clock out note"
                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) "clock out note"))))))

      (it "clock note (file)"
        (let* ((ts0 "[2112-01-01 Fri 00:00]")
               (ts1 "[2112-01-02 Sat 01:00]")
               (clock (format "CLOCK: %s--%s => 1:00" ts0 ts1)))
          (expect-sql-tbls (clocks) (list "#+STARTUP: lognoteclock-out"
                                          "* parent"
                                          ":LOGBOOK:"
                                          clock
                                          "- clock out note"
                                          ":END:")
                           `((clocks (1 1 ,(org-ts-to-unixtime ts0)
                                        ,(org-ts-to-unixtime ts1) "clock out note")))))))))

(defun format-with (config type value)
  (funcall (org-sql--compile-value-format-function config type) value))

(defun format-with-sqlite (type value)
  (format-with '(sqlite) type value))

(defun expect-formatter (type input &rest value-plist)
  (declare (indent 2))
  (-let (((&plist :sqlite :postgres :mysql :sqlserver) value-plist))
    (expect (format-with '(mysql) type input) :to-equal mysql)
    (expect (format-with '(postgres) type input) :to-equal postgres)
    (expect (format-with '(sqlite) type input) :to-equal sqlite)
    (expect (format-with '(sqlserver) type input) :to-equal sqlserver)))

(describe "type formatting spec"
  (describe "boolean"
    (it "NULL"
      (expect-formatter 'boolean nil
        :mysql "NULL"
        :postgres "NULL"
        :sqlite "NULL"
        :sqlserver "NULL"))

    (it "TRUE"
      (expect-formatter 'boolean 1
        :mysql "TRUE"
        :postgres "TRUE"
        :sqlite "1"
        :sqlserver "1"))

    (it "FALSE"
      (expect-formatter 'boolean 0
        :mysql "FALSE"
        :postgres "FALSE"
        :sqlite "0"
        :sqlserver "0")))

  (describe "enum"
    (it "NULL"
      (expect-formatter 'enum nil
        :mysql "NULL"
        :postgres "NULL"
        :sqlite "NULL"
        :sqlserver "NULL"))

    (it "defined"
      (expect-formatter 'enum 'foo
        :mysql "'foo'"
        :postgres "'foo'"
        :sqlite "'foo'"
        :sqlserver "'foo'")))

  ;; ASSUME this is the same as REAL
  (describe "integer"
    (it "NULL"
      (expect-formatter 'integer nil
        :mysql "NULL"
        :postgres "NULL"
        :sqlite "NULL"
        :sqlserver "NULL"))

    (it "defined"
      (expect-formatter 'integer 123456
        :mysql "123456"
        :postgres "123456"
        :sqlite "123456"
        :sqlserver "123456")))

  ;; ASSUME this is the same as VARCHAR and CHAR
  (describe "text"
    (it "NULL"
      (expect-formatter 'text nil
        :mysql "NULL"
        :postgres "NULL"
        :sqlite "NULL"
        :sqlserver "NULL"))

    (it "plain"
      (expect-formatter 'text "foo"
        :mysql "'foo'"
        :postgres "'foo'"
        :sqlite "'foo'"
        :sqlserver "'foo'"))
    
    (it "newlines"
      (expect-formatter 'text "foo\nbar"
        :mysql "'foo\\\\nbar'"
        :postgres "'foo'||chr(10)||'bar'"
        :sqlite "'foo'||char(10)||'bar'"
        :sqlserver "'foo+Char(10)+bar'"))

    (it "quotes"
      (expect-formatter 'text "'foo'"
        :mysql "'\\\\'foo\\\\''"
        :postgres "'''foo'''"
        :sqlite "'''foo'''"
        :sqlserver "'''foo'''"))))

(describe "meta-query language statement formatting spec"
  (before-all
    (setq org-sql--table-alist
          '((table-foo
             (columns
              (:bool :type boolean)
              (:enum :type enum :allowed (bim bam boo))
              (:int :type integer)
              (:text :type text))
             (constraints
              (primary :keys (:int))))
            (table-bar
             (columns
              (:intone :type integer)
              (:inttwo :type integer))
             (constraints
              (primary :keys (:intone))
              (foreign :ref table-foo
                       :keys (:inttwo)
                       :parent-keys (:int)
                       ;; :on_update cascade
                       :on-delete cascade))))
          test-insert-alist
          '((table-foo (0 bim 0 "xxx") (1 bam 1 "yyy"))
            (table-bar (0 1) (2 3)))))

  (describe "bulk insert"
    (it "SQLite"
      (let ((config '(sqlite)))
        (expect
         (org-sql--format-bulk-inserts config test-insert-alist)
         :to-equal
         (concat "INSERT INTO table-foo (bool,enum,int,text) VALUES (0,'bim',0,'xxx'),(1,'bam',1,'yyy');"
                 "INSERT INTO table-bar (intone,inttwo) VALUES (0,1),(2,3);"))))

    (it "Postgres"
      (let ((config '(postgres)))
        (expect
         (org-sql--format-bulk-inserts config test-insert-alist)
         :to-equal
         (concat "INSERT INTO table-foo (bool,enum,int,text) VALUES (FALSE,'bim',0,'xxx'),(TRUE,'bam',1,'yyy');"
                 "INSERT INTO table-bar (intone,inttwo) VALUES (0,1),(2,3);"))))

    (it "MySQL"
      (let ((config '(mysql)))
        (expect
         (org-sql--format-bulk-inserts config test-insert-alist)
         :to-equal
         (concat "INSERT INTO table-foo (bool,enum,int,text) VALUES (FALSE,'bim',0,'xxx'),(TRUE,'bam',1,'yyy');"
                 "INSERT INTO table-bar (intone,inttwo) VALUES (0,1),(2,3);"))))

    (it "SQL-Server"
      (let ((config '(sqlserver)))
        (expect
         (org-sql--format-bulk-inserts config test-insert-alist)
         :to-equal
         (concat "INSERT INTO table-foo (bool,enum,int,text) VALUES (0,'bim',0,'xxx'),(1,'bam',1,'yyy');"
                 "INSERT INTO table-bar (intone,inttwo) VALUES (0,1),(2,3);")))))

  ;; TODO add bulk deletes...eventually

  (describe "create table"
    (it "SQLite"
      (let ((config '(sqlite)))
        (expect
         (org-sql--format-create-tables config org-sql--table-alist)
         :to-equal
         (list
          "CREATE TABLE IF NOT EXISTS table-foo (bool INTEGER,enum TEXT,int INTEGER,text TEXT,PRIMARY KEY (int));"
          "CREATE TABLE IF NOT EXISTS table-bar (intone INTEGER,inttwo INTEGER,PRIMARY KEY (intone),FOREIGN KEY (inttwo) REFERENCES table-foo (int) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED);"))))

    (it "postgres"
      (let ((config '(postgres)))
        (expect
         (org-sql--format-create-tables config org-sql--table-alist)
         :to-equal
         (list
          "CREATE TYPE enum_table-foo_enum AS ENUM ('bim','bam','boo');"
          "CREATE TABLE IF NOT EXISTS table-foo (bool BOOLEAN,enum enum_table-foo_enum,int INTEGER,text TEXT,PRIMARY KEY (int));"
          "CREATE TABLE IF NOT EXISTS table-bar (intone INTEGER,inttwo INTEGER,PRIMARY KEY (intone),FOREIGN KEY (inttwo) REFERENCES table-foo (int) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED);"))))

    (it "postgres - unlogged"
      (let ((config '(postgres :unlogged t)))
        (expect
         (org-sql--format-create-tables config org-sql--table-alist)
         :to-equal
         (list
          "CREATE TYPE enum_table-foo_enum AS ENUM ('bim','bam','boo');"
          "CREATE UNLOGGED TABLE IF NOT EXISTS table-foo (bool BOOLEAN,enum enum_table-foo_enum,int INTEGER,text TEXT,PRIMARY KEY (int));"
          "CREATE UNLOGGED TABLE IF NOT EXISTS table-bar (intone INTEGER,inttwo INTEGER,PRIMARY KEY (intone),FOREIGN KEY (inttwo) REFERENCES table-foo (int) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED);"))))

    (it "postgres - nonpublic"
      (let ((config '(postgres :schema "nonpublic")))
        (expect
         (org-sql--format-create-tables config org-sql--table-alist)
         :to-equal
         (list
          "CREATE TYPE nonpublic.enum_table-foo_enum AS ENUM ('bim','bam','boo');"
          "CREATE TABLE IF NOT EXISTS nonpublic.table-foo (bool BOOLEAN,enum nonpublic.enum_table-foo_enum,int INTEGER,text TEXT,PRIMARY KEY (int));"
          "CREATE TABLE IF NOT EXISTS nonpublic.table-bar (intone INTEGER,inttwo INTEGER,PRIMARY KEY (intone),FOREIGN KEY (inttwo) REFERENCES nonpublic.table-foo (int) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED);"))))

    (it "mysql"
      (let ((config '(mysql)))
        (expect
         (org-sql--format-create-tables config org-sql--table-alist)
         :to-equal
         (list
          "CREATE TABLE IF NOT EXISTS table-foo (bool BOOLEAN,enum ENUM('bim','bam','boo'),int INTEGER,text TEXT,PRIMARY KEY (int));"
          "CREATE TABLE IF NOT EXISTS table-bar (intone INTEGER,inttwo INTEGER,PRIMARY KEY (intone),FOREIGN KEY (inttwo) REFERENCES table-foo (int) ON DELETE CASCADE);"))))

    (it "sqlserver"
      (let ((config '(sqlserver)))
        (expect
         (org-sql--format-create-tables config org-sql--table-alist)
         :to-equal
         (list
          "IF NOT EXISTS (SELECT * FROM sys.tables where name = 'table-foo') CREATE TABLE table-foo (bool BIT,enum NVARCHAR(MAX),int INTEGER,text NVARCHAR(MAX),PRIMARY KEY (int));"
          "IF NOT EXISTS (SELECT * FROM sys.tables where name = 'table-bar') CREATE TABLE table-bar (intone INTEGER,inttwo INTEGER,PRIMARY KEY (intone),FOREIGN KEY (inttwo) REFERENCES table-foo (int) ON DELETE CASCADE);"))))

    (it "sqlserver - nonpublic"
      (let ((config '(sqlserver :schema "nonpublic")))
        (expect
         (org-sql--format-create-tables config org-sql--table-alist)
         :to-equal
         (list
          "IF NOT EXISTS (SELECT * FROM sys.tables where name = 'nonpublic.table-foo') CREATE TABLE nonpublic.table-foo (bool BIT,enum NVARCHAR(MAX),int INTEGER,text NVARCHAR(MAX),PRIMARY KEY (int));"
          "IF NOT EXISTS (SELECT * FROM sys.tables where name = 'nonpublic.table-bar') CREATE TABLE nonpublic.table-bar (intone INTEGER,inttwo INTEGER,PRIMARY KEY (intone),FOREIGN KEY (inttwo) REFERENCES nonpublic.table-foo (int) ON DELETE CASCADE);")))))

  (describe "transaction"
    (it "sqlite"
      (let ((config '(sqlite))
            (statements (list "INSERT INTO foo (bar) values (1);")))
        (expect
         (org-sql--format-sql-transaction config statements)
         :to-equal
         "PRAGMA foreign_keys = ON;BEGIN;INSERT INTO foo (bar) values (1);COMMIT;")))

    (it "postgres"
      (let ((config '(postgres))
            (statements (list "INSERT INTO foo (bar) values (1);")))
        (expect
         (org-sql--format-sql-transaction config statements)
         :to-equal
         "BEGIN;INSERT INTO foo (bar) values (1);COMMIT;")))

    (it "mysql"
      (let ((config '(mysql))
            (statements (list "INSERT INTO foo (bar) values (1);")))
        (expect
         (org-sql--format-sql-transaction config statements)
         :to-equal
         "BEGIN;INSERT INTO foo (bar) values (1);COMMIT;")))

    (it "sqlserver"
      (let ((config '(sqlserver))
            (statements (list "INSERT INTO foo (bar) values (1);")))
        (expect
         (org-sql--format-sql-transaction config statements)
         :to-equal
         "BEGIN TRANSACTION;INSERT INTO foo (bar) values (1);COMMIT;")))))

(describe "file metadata spec"
  (it "classify file metadata"
    (let ((on-disk '(("123" . "/bar.org")
                     ("654" . "/bam.org")
                     ("456" . "/foo.org")))
          (in-db '(("123" . "/bar.org")
                   ("654" . "/bam0.org")
                   ("789" . "/foo0.org"))))
      (expect (org-sql--partition-hashpathpairs on-disk in-db)
              :to-equal
              '((files-to-insert ("456" . "/foo.org"))
                (paths-to-insert ("654" . "/bam.org"))
                (paths-to-delete ("654" . "/bam0.org"))
                (files-to-delete ("789" . "/foo0.org")))))))

;;; org-sql-test-stateless.el ends here

