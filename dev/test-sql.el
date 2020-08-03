(require 'org-sql)
(require 'buttercup)

(describe "First spec"
  (it "is my first test using buttercup, be nice ;)"
    (let ((res (org-sql--fmt-pragma '(:one zero))))
      (expect res :to-equal "PRAGMA one=zero;"))))

