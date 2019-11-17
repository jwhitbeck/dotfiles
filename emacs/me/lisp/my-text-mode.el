;;; -*- lexical-binding: t; -*-

;;;; Text-mode settings

;;; Turn on undo-tree mode
(add-hook 'text-mode-hook 'undo-tree-mode)

;;; Turn on flyspell
;;; Enable spell checking
(add-hook 'text-mode-hook 'flyspell-mode)

;;; Turn on auto revert mode
;; automatically revert a buffer when a file is changed on disk
(add-hook 'text-mode-hook 'auto-revert-mode)

(provide 'my-text-mode)
