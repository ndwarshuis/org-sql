;;; test-helper.el --- Helpers for org-sql-test.el

;;; test-helper.el ends here

(require 'f)

(defconst test-root (f-dirname (f-this-file)))
(defconst code-root (f-parent test-root))

(require 'org-sql (f-expand "org-sql.el" code-root))

(defmacro with-sandbox (&rest body)
  "Run BODY in a sandboxed Org directory."
  `(let ((org-directory (f-join test-root "Org")))
     ,@body))
