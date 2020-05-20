;;; -*- lexical-binding: t; -*-

;;;; Flyspell configuration
(require 'flyspell)

;;; Don't let flyspell rebind C-c $, which I use in org-mode.
(define-key flyspell-mode-map (kbd "C-c $") nil)

(provide 'my-flyspell)
