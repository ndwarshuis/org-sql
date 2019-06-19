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
                                "error.org")
                              (--map (f-join org-directory it))))
          (real-files (f-files org-directory nil t))
          (fake-files (->> '("fake.org" "fake.org")
                           (--map (f-join org-directory it))))
          (org-sql-files (append real-files fake-files)))
     (should (eq '() (-difference (org-sql-files) real-files))))))
