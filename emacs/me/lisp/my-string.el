;;; -*- lexical-binding: t; -*-

;;;; String helper functions

;;; Simple functions for replacing certain chars in a string with another. Emacs
;;; doesn't appear to have such functions built-in.

;;;###autoload
(defun my-replace-string (s old new)
  "Replace occurrences of 'old' string within string 's' with
  'new' string."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (search-forward old nil t)
      (replace-match new))
    (buffer-string)))

;;;###autoload
(defun my-replace-string-re (s re new)
  "Replace matches of regexp within string 's' with 'new'
  string."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (search-forward-regexp re nil t)
      (replace-match new))
    (buffer-string)))

(provide 'my-string)
