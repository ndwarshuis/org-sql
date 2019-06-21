;;; org-sql-test.el --- Tests for org-sql

;;; org-sql-test.el ends here

(require 'dash)

;; TODO ...wettest code ever :(
(ert-deftest org-sql/files-all ()
  "Should return all files from `org-sql-files'."
  (with-sandbox
   (let ((org-sql-files (->> '("arch1.org_archive"
                               "subdir"
                               "test1.org"
                               "error.org")
                             (--map (f-join org-directory it))))
         (real-files (f-files org-directory nil t)))
     (should (eq '() (-difference (org-sql-files) real-files))))))

(ert-deftest org-sql/files-exist ()
  "Should return all files from `org-sql-files' but only those that
exist."
  (with-sandbox
   (let* ((org-sql-files (->> '("arch1.org_archive"
                                "subdir"
                                "test1.org"
                                "error.org"
                                "fake.org")
                              (--map (f-join org-directory it))))
          (real-files (f-files org-directory nil t)))
     (should (eq '() (-difference (org-sql-files) real-files))))))

;; TODO ...there's a better way to do this, right?
(ert-deftest org-sql/extract-files ()
  "Should return a valid accumulator."
  (with-sandbox
   (let* ((test-file (f-join org-directory "test1.org"))
          (test-file-size (f-size test-file))
          (test-md5-sum "some-md5")
          (test-cell (cons test-file test-md5-sum)))
     (should (equal
              `((files
                 ,(list :file_path test-file
                        :md5 test-md5-sum
                        :size test-file-size))
                (headlines
                 ,(list :file_path test-file
                        :headline_offset 1
                        :tree_path nil
                        :headline_text "small test headline"
                        :keyword nil
                        :effort nil
                        :priority nil
                        :archived 0
                        :commented 0
                        :content nil)))
              (org-sql--extract-file test-cell nil))))))

(ert-deftest org-sql/string-string-nil ()
  "Should return nil when given nil."
  (should-not (org-sql--strip-string nil)))

(ert-deftest org-sql/string-string-extra-spaces ()
  "Should return a string with all extra whitespace trimmed."
  (should (equal "fxmldr" (org-sql--strip-string " \t   fxmldr \n "))))

(ert-deftest org-sql/string-string-extra-properties ()
  "Should return a string with text properties removed."
  (should (equal "test" (org-sql--strip-string
                         (propertize "test" :one 1)))))

(ert-deftest org-sql/alist-put-nil ()
  "Should return a new alist if none is supplied."
  (should (equal '((:one 1)) (org-sql--alist-put nil :one 1))))

(ert-deftest org-sql/alist-put-add ()
  "Should add new value to existing slot in alist if it exists.
Will be added to the front of the slot."
  (let ((res '((:one 1 2))))
    (should (equal res (org-sql--alist-put '((:one 2)) :one 1)))
    (should-not (equal res (org-sql--alist-put '((:one 1)) :one 2)))))

(ert-deftest rg-sql/alist-put-new ()
  "Should make a new slot with new value if slot does not exist.
New cell goes in the back of the alist."
  (let ((res '((:one 1) (:two 2))))
    (should (equal res (org-sql--alist-put '((:one 1)) :two 2)))
    (should-not (equal res (org-sql--alist-put '((:two 2)) :one 1)))))

(ert-deftest rg-sql/alist-put-current ()
  "Should add new value into current cell if it has no cdr."
    (should (equal '((:one 1)) (org-sql--alist-put '((:one)) :one 1))))

  
(ert-deftest org-sql/plist-get-keys-valid ()
  "Should return the keys of a plist."
  (should (equal '(:one two "three") (org-sql--plist-get-keys
                                      '(:one 1 two 2 "three" 3)))))

(ert-deftest org-sql/plist-get-keys-nil ()
  "Should return nil if no plist given."
  (should-not (org-sql--plist-get-keys nil)))

(ert-deftest org-sql/plist-get-vals-valid ()
  "Should return the values of a plist."
  (should (equal '(1 "2" :3) (org-sql--plist-get-vals
                              '(:one 1 :two "2" :three :3)))))

(ert-deftest org-sql/plist-get-vals-nil ()
  "Should return nil if no plist given."
  (should-not (org-sql--plist-get-vals nil)))

(ert-deftest org-sql/to-plist-blank ()
  "Should return nothing if given an empty string or nil."
  (should-not (org-sql--to-plist "" '())))

(ert-deftest org-sql/to-plist-nil ()
  "Should given an error if given nil."
  (should-error (org-sql--to-plist nil '())))

(ert-deftest org-sql/to-plist-valid ()
  "Should give a list of plists for a given SQL-formatted input.
Input might be multiple lines."
  (should (equal '((:one "1" :two "2" :three "3")
                   (:one "4" :two "5" :three "6"))
                   (org-sql--to-plist "1|2|3\n4|5|6"
                                    '(:one :two :three)))))

(ert-deftest org-sql/escape-text-nil ()
  "Should give an error if given nil"
  (should-error (org-sql--escape-text nil)))

(ert-deftest org-sql/escape-text-blank ()
  "Should return a single-quoted blank if given a blank."
  (should (equal "''" (org-sql--escape-text ""))))

(ert-deftest org-sql/escape-text-newline ()
  "Should insert a '||char(10)||' for every \n character."
  (should (equal "''||char(10)||''" (org-sql--escape-text "\n"))))

(ert-deftest org-sql/escape-text-single-quote ()
  "Should insert two single quotes for every quote."
  (should (equal "''''" (org-sql--escape-text "'"))))

(ert-deftest org-sql/to-string-nil ()
  "Should return \"NULL\" when given nil."
  (should (equal "NULL" (org-sql--to-string nil))))

(ert-deftest org-sql/to-string-keyword ()
  "Should return a stringified keyword without the leading ':'."
  (should (equal "terrordome" (org-sql--to-string :terrordome))))

(ert-deftest org-sql/to-string-string ()
  "Should return an escaped string when given a string."
  (let ((s "'a'\n'b'"))
    (should (equal (org-sql--escape-text s) (org-sql--to-string s)))))

(ert-deftest org-sql/to-string-number ()
  "Should return a stringified number when given a number."
  (should (equal "1" (org-sql--to-string 1))))

(ert-deftest org-sql/to-string-list ()
  "Should return an error if given a list."
  (should-error (org-sql--to-string '(1 2 3))))

(ert-deftest org-sql/to-string-symbol ()
  "Should return the symbol's escaped name when given a symbol."
  (should (equal (org-sql--escape-text "abc")
                 (org-sql--to-string 'abc))))

(ert-deftest org-sql/kw-to-colname-nil ()
  "Should return error when given nil."
  (should-error (org-sql--kw-to-colname nil)))

(ert-deftest org-sql/kw-to-colname-non-keyword ()
  "Should return error when not given a keyword."
  (should-error (org-sql--kw-to-colname 1))
  (should-error (org-sql--kw-to-colname "1"))
  (should-error (org-sql--kw-to-colname 'a))
  (should-error (org-sql--kw-to-colname '())))

(ert-deftest org-sql/kw-to-colname-keyword ()
  "Should return error when given nil."
  (should (equal "yeah-boi" (org-sql--kw-to-colname :yeah-boi))))

(ert-deftest org-sql/plist-concat-nil ()
  "Should return nil if not given anything, with or without a sep."
  (should-not (org-sql--plist-concat nil))
  (should-not (org-sql--plist-concat nil ", ")))

(ert-deftest org-sql/plist-concat-valid ()
  "Should return concatenated plist when given one with "
  (should (equal "one=1,two='2',three=3,four=4"
                 (org-sql--plist-concat
                  (list :one 1 :two "2" :three '3 :four :4)))))

(ert-deftest org-sql/effort-to-int-nil ()
  "Should return nil if given nil."
  (should-not (org-sql--effort-to-int nil)))

(ert-deftest org-sql/effort-to-int-valid ()
  "Should return minutes as int of converted time."
  (should (eq 30 (org-sql--effort-to-int "0:30")))
  (should (eq 90 (org-sql--effort-to-int "1:30"))))

(ert-deftest org-sql/effort-to-int-valid-string ()
  "Should return minutes as string of converted time."
  (should (equal "30" (org-sql--effort-to-int "0:30" t)))
  (should (equal "90" (org-sql--effort-to-int "1:30" t))))

(ert-deftest org-sql/effort-to-int-invalid ()
  "Invalid strings should return nil."
  (should-not (org-sql--effort-to-int "" nil))
  (should-not (org-sql--effort-to-int ":30" nil))
  (should-not (org-sql--effort-to-int "-0:30" nil))
  (should-not (org-sql--effort-to-int "0:" nil)))

(ert-deftest org-sql/effort-to-int-invalid-throw ()
  "Invalid strings should throw error if we want it."
  (should-error (org-sql--effort-to-int "" nil t))
  (should-error (org-sql--effort-to-int ":30" nil t))
  (should-error (org-sql--effort-to-int "-0:30" nil t))
  (should-error (org-sql--effort-to-int "0:" nil t)))

(ert-deftest org-sql/ts-fmt-unix-time-nil ()
  "Should return nil if given nil."
  (should-not (org-sql--ts-fmt-unix-time nil)))

(ert-deftest org-sql/ts-fmt-unix-time-valid ()
  "Should return a unixtime integer if given valid timestamp."
  (cl-flet ((date-local-time
             (d m y)
             (-> (encode-time 0 0 0 d m y (current-time-zone))
                 (float-time)
                 (round))))
    (should (= (date-local-time 8 6 2012)
               (org-sql--ts-fmt-unix-time "[2012-06-08]")))
    (should (= (date-local-time 29 7 2016)
               (org-sql--ts-fmt-unix-time "[2016-07-29]")))
    (should (= (date-local-time 14 6 2019)
               (org-sql--ts-fmt-unix-time "[2019-06-14]")))))

(ert-deftest org-sql/ts-fmt-unix-time-invalid ()
  "Should return nil if given an invalid timestamp."
  (should-not (org-sql--ts-fmt-unix-time ""))
  (should-not (org-sql--ts-fmt-unix-time "201-06-14")))

(ert-deftest org-sql/parse-ts-range-nil ()
  "Should return nil if given nil."
  (should-not (org-sql--parse-ts-range nil)))

;; TODO this should be refactored to make the resolution separate
;; (ert-deftest org-sql/parse-ts-range-single ()
;;   "Should return a length-1 list with a time."
;;   (let ((s (string-to-org-element "[2019-06-14 00:00]"))
;;   (should-not (org-sql--parse-ts-range nil)))

(ert-deftest org-sql/todo-keywords-default ()
  "Should return a list of TODO keyword strings."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (should (equal '("TODO" "DONE") (org-sql--todo-keywords)))))

(ert-deftest org-sql/todo-keywords-default-selectors ()
  "Should return a list of TODO keyword strings without selectors."
  (let ((org-todo-keywords '((sequence "TODO(t/!)" "DONE(d/!)"))))
    (should (equal '("TODO" "DONE") (org-sql--todo-keywords)))))

(ert-deftest org-sql/todo-keywords-default-two ()
  "Should return a list of flattened TODO keyword strings."
  (let ((org-todo-keywords '((sequence "TODO" "DONE")
                             (sequence "WAIT" "CANC"))))
    (should (equal '("TODO" "DONE" "WAIT" "CANC")
                   (org-sql--todo-keywords)))))

(ert-deftest org-sql/todo-keywords-default-pipe ()
  "Should return a list of TODO keyword strings without pipes."
  (let ((org-todo-keywords '((sequence "TODO" "DONE" "|" "CANC"))))
    (should (equal '("TODO" "DONE" "CANC") (org-sql--todo-keywords)))))
