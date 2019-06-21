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
  
(ert-deftest org-sql/plist-get-keys-valid ()
  "Should return the keys of a plist or nil if no plist given."
  (should (equal '(:one two "three")
                 (org-sql--plist-get-keys
                  '(:one 1 two 2 "three" 3)))))

(ert-deftest org-sql/plist-get-keys-nil ()
  "Should return the keys of a plist or nil if no plist given."
  (should-not (org-sql--plist-get-keys nil)))
