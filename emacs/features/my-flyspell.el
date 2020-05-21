;;; -*- lexical-binding: t; -*-

;;;; Flyspell configuration
(require 'flyspell)
(require 'ispell)

;;; Don't let flyspell rebind C-c $, which I use in org-mode.
(define-key flyspell-mode-map (kbd "C-c $") nil)

;;; Disable minor mode indicator
(setq flyspell-mode-line-string nil)

;;; Default to american dictionary
(setq ispell-dictionary "american")

;;; Fast dictionary toggle
(defun my-ispell-toggle-dictionary ()
  (interactive)
  (ispell-change-dictionary
   (pcase (or ispell-local-dictionary ispell-dictionary)
     ("american" "french")
     ("french" "american")
     (_ "american"))))

(define-key flyspell-mode-map  (kbd "C-c d") 'my-ispell-toggle-dictionary)

(provide 'my-flyspell)
