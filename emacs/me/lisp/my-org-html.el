;;; -*- lexical-binding: t; -*-

;;; Org HTML export customizations

(eval-when-compile
  (require 'ox-html))

(require 'my-package)
(my-use-packages htmlize)

;;; Online resources
;;; - https://emacs.stackexchange.com/questions/7629/the-syntax-highlight-and-indentation-of-source-code-block-in-exported-html-file
;;; - https://vxlabs.com/2015/01/28/sending-emails-with-math-and-source-code/
(defconst my-css-dir (expand-file-name "css" my-dir))
(defconst my-js-dir (expand-file-name "js" my-dir))

(defun my-build-html-head ()
  "Returns a string of inline css to use in org-mode's html export."
  (let ((css-files (mapcar (lambda (file) (expand-file-name file my-css-dir))
                           '("bootstrap.min.css"
                             "bootstrap.diff.css"
                             "code.css"
                             "org-html-export.css"))))
    (with-temp-buffer
      (insert "<style type=\"text/css\">\n")
      (dolist (file css-files)
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "\n"))
      (insert "</style>\n")
      (buffer-string))))

(defun my-build-html-scripts ()
  "Returns a string of line javascript for use in org-modes's html export."
  (with-temp-buffer
    (insert "<script type=\"text/javascript\">\n")
    (insert-file-contents (expand-file-name "org.js" my-js-dir))
    (goto-char (point-max))
    (insert "</script>\n")
    (buffer-string)))

(defconst my-org-html-postamble
  (concat "<dl class=\"dl-horizontal\">\n"
          "  <dt>Author</dt><dd>%a</dd>\n"
          "  <dt>Updated</dt><dd>%C</dd>\n"
          "</dl>\n"))

(custom-set-variables
 '(org-export-babel-evaluate 'inline-only)
 '(org-html-doctype "html5")
 '(org-html-html5-fancy t)
 '(org-html-head-include-default-style nil)
 '(org-html-postamble t)
 '(org-html-postamble-format (("en" my-org-html-postamble)))
 `(org-html-scripts ,(my-build-html-scripts))
 `(org-html-head ,(my-build-html-head))
 '(org-html-htmlize-output-type 'css)
 '(org-html-htmlize-font-prefix "org-")
 '(org-export-with-toc nil))

;;; Fix issue where fill-column-indicator outputs unprintable characters at the end of code blocks
;;; https://github.com/alpaker/Fill-Column-Indicator/issues/45
(defun fci-mode-override-advice (&rest args))
(advice-add 'org-html-fontify-code :around
            (lambda (fun &rest args)
              (advice-add 'fci-mode :override 'fci-mode-override-advice)
              (let ((result  (apply fun args)))
                (advice-remove 'fci-mode 'fci-mode-override-advice)
                result)))

(provide 'my-org-html)
