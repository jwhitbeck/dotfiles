;;; -*- lexical-binding: t; -*-

;;; ispell helpers

(require 'ispell)

;;; Default to american dictionary
(setq ispell-dictionary "american")

(defun my-ispell-toggle-dictionary ()
  (interactive)
  (ispell-change-dictionary
   (pcase (or ispell-local-dictionary ispell-dictionary)
     ("american" "french")
     ("french" "american")
     (_ "american"))))

(define-key ispell-minor-keymap (kbd "C-c d") 'my-ispell-toggle-dictionary)

(provide 'my-ispell)
