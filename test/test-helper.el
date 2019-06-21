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

(defun string-to-org-element (s)
  "Convert S to org element. This assume S is valid org syntax."
  (cl-labels
      ((get-contents
        (elem)
        (let ((type (org-element-type elem)))
          (if (or (eq type 'paragraph) (eq type 'section))
              (get-contents (car (org-element-contents elem)))
            elem))))
    (with-temp-buffer
      (insert s)
      (->> (org-element-parse-buffer)
           (org-element-contents)
           (car)
           (get-contents)))))
         
