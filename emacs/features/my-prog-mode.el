;;; -*- lexical-binding: t; -*-

;;;; Settings for programming major modes
(require 'prog-mode)

;;; Activate rainbow delimeters in prog mode.
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Show a bar beyond the fill-column
(add-hook 'prog-mode-hook 'fci-mode)

;;; Enable Flycheck in all program editing modes
(add-hook 'prog-mode-hook 'flycheck-mode)

;;; Activate undo tree for all program editing modes.
(add-hook 'prog-mode-hook 'undo-tree-mode)

;;; Activate flyspell for comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Automatically revert a buffer when a file is changed on disk.
(add-hook 'prog-mode-hook 'auto-revert-mode)

(provide 'my-prog-mode)
