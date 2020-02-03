;;; -*- lexical-binding: t; -*-

;;;; Flyspell configuration
(require 'flyspell)
(require 'auto-dictionary)

;;; Don't let flyspell rebind C-c $, which I use in org-mode.
(define-key flyspell-mode-map (kbd "C-c $") nil)

;;; Enable auto-dictionary to automatically detect English or French.
(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))

(provide 'my-flyspell)
