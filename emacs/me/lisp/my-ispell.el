;;; -*- lexical-binding: t; -*-

;;; ispell helpers

(require 'ispell)
(defvar my-ispell-dictionaries '("american" "french"))

;;; Default to american dictionary
(ispell-change-dictionary "american")

;;;###autoload
(defun my-ispell-toggle-dictionary ()
  (interactive)
  (ispell-change-dictionary
   (pcase ispell-local-dictionary
     ("american" "french")
     ("french" "american")
     (_ "american"))))

(provide 'my-ispell)
