(require 'org-sql)
(require 's)
(require 'buttercup)

;;; Code:

(defun list-to-lines (in)
  "Convert IN to string.
If IN is a string, return IN. If IN is a list starting with
:lines then join the cdr of IN with newlines."
  (cond
   ((stringp in) in)
   ((equal (car in) :lines)
    (s-join "\n" (cdr in)))
   (t (error "String or list of strings expected"))))

(defconst testing-filepath "/tmp/dummy")

(defmacro expect-sql (in tbl)
  (declare (indent 1))
  (let ((in (list-to-lines in)))
  `(progn
     (insert ,in)
     (let ((res (org-sql--extract-buffer nil testing-filepath)))
       (expect res :to-equal ,tbl)))))

(describe "SQL metalangage spec"
  (before-all
    (org-mode))

  (before-each
    (erase-buffer))

  (it "single headline"
    (expect-sql "* headline"
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path nil
                :headline_text "headline"
                :keyword nil
                :effort nil
                :priority nil
                :archived 0
                :commented 0
                :content nil)))))

  (it "two headlines"
    ;; NOTE reverse order
    (expect-sql (:lines "* headline"
                        "* another headline")
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 12
                :tree_path nil
                :headline_text "another headline"
                :keyword nil
                :effort nil
                :priority nil
                :archived 0
                :commented 0
                :content nil)
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path nil
                :headline_text "headline"
                :keyword nil
                :effort nil
                :priority nil
                :archived 0
                :commented 0
                :content nil)))))

  (it "fancy headline"
    (expect-sql (:lines "* TODO [#A] COMMENT another headline"
                        ":PROPERTIES:"
                        ":Effort: 0:30"
                        ":END:")
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path nil
                :headline_text "another headline"
                :keyword "TODO"
                :effort "30"
                :priority "A"
                :archived 0
                :commented 1
                :content nil)))))

  (it "nested headline"
    (expect-sql (:lines "* headline"
                        "** nested headline")
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 12
                :tree_path "/headline"
                :headline_text "nested headline"
                :keyword nil
                :effort nil
                :priority nil
                :archived 0
                :commented 0
                :content nil)
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path nil
                :headline_text "headline"
                :keyword nil
                :effort nil
                :priority nil
                :archived 0
                :commented 0
                :content nil)))))

  (it "tagged headline"
    (expect-sql "* headline :sometag:"
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path nil
                :headline_text "headline"
                :keyword nil
                :effort nil
                :priority nil
                :archived 0
                :commented 0
                :content nil))
        (tags
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tag "sometag"
                :inherited 0)))))

  (it "archived headline"
    (expect-sql "* headline :ARCHIVE:"
      `((headlines
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tree_path nil
                :headline_text "headline"
                :keyword nil
                :effort nil
                :priority nil
                :archived 1
                :commented 0
                :content nil))
        (tags
         ,(list :file_path testing-filepath
                :headline_offset 1
                :tag "ARCHIVE"
                :inherited 0))))))
