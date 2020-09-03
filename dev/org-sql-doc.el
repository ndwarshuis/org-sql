;;; org-sql-doc.el --- Build documentation for org-sql

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

;;; Code:

(defvar org-sql-dev-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to development directory.")

(defvar org-sql-dev-root-path
  (directory-file-name (file-name-directory org-sql-dev-path))
  "Path to root directory.")

(add-to-list 'load-path org-sql-dev-root-path)
(add-to-list 'load-path org-sql-dev-path)

(require 'package)
(require 'dash)
(require 's)
(require 'org-sql)

(defun org-sql-get-package-version ()
  "Get version of om package."
  (with-current-buffer (find-file-noselect "org-sql.el")
    (->> (package-buffer-info)
         (package-desc-version)
         (-map 'number-to-string)
         (s-join version-separator))))

;; (let ((public-syms (alist-get 'public org-ml-dev-defined-names))
;;       (example-syms (->> (-remove #'stringp org-ml-dev-examples-list)
;;                          (-map #'car))))
;;   (-some->> (-difference public-syms example-syms)
;;             (-map #'symbol-name)
;;             (--remove (s-ends-with? "*" it))
;;             (--remove (s-starts-with? "org-ml-update-this-" it))
;;             (--remove (s-starts-with? "org-ml-parse-this-" it))
;;             (--map (format "  %s" it))
;;             (s-join "\n")
;;             (format "The following functions don't have examples:\n%s")
;;             (print)))

(defun goto-and-replace-all (s replacement)
  (while (progn (goto-char (point-min)) (search-forward s nil t))
    (delete-char (- (length s)))
    (insert replacement)))

(defun create-docs-file ()
  ;; (let ((org-ml-dev-examples-list (nreverse org-ml-dev-examples-list)))
    (with-temp-file "./README.md"
      (insert-file-contents-literally "./readme-template.md")

      ;; (goto-and-remove "[[ function-list ]]")
      ;; (insert (mapconcat 'function-summary org-ml-dev-examples-list "\n"))

      ;; (goto-and-remove "[[ function-docs ]]")
      ;; (insert (mapconcat 'function-to-md org-ml-dev-examples-list "\n"))

      (goto-and-replace-all "[[ version ]]" (org-sql-get-package-version))))

      ;; (simplify-quotes))))

;;; org-sql-doc.el ends here
