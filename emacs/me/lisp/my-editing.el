;;; -*- lexical-binding: t; -*-

;;; General editing customizations

(require 'my-package)

(my-use-packages auto-indent-mode undo-tree)

(custom-set-variables
 '(auto-save-default nil)               ; disable autosave
 '(make-backup-files nil))              ; disable auto backups
(global-auto-revert-mode t)             ; automatically revert a buffer when a file is changed on disk
(global-undo-tree-mode t)               ; always activate undo tree

;;; Highlight and auto-correct whitespace problems
(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(custom-set-variables
 '(whitespace-style '(face empty trailing tabs tab-mark)))

;;; Don't highlight tabs in whitespace mode
(defun my-disable-tab-highlighting ()
  "Disable tab highlighting in whitespace mode."
  (make-local-variable 'whitespace-style)
  (setq whitespace-style (delq 'tabs whitespace-style))
  (setq whitespace-style (delq 'tab-mark whitespace-style)))

;;; No tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;;; Makefile-mode already does that, for example.
(custom-set-variables
 '(indent-tabs-mode nil))

(defun my-enable-indent-tabs ()
  "Enable using tabs for indentation."
  (setq indent-tabs-mode t))

;;; If indent-tabs-mode is off, untabify before saving.
(defun my-untabify-buffer ()
  "Replace all tabs with spaces in buffer."
  (when (not indent-tabs-mode)
    (untabify (point-min) (point-max)))
  nil)

(add-hook 'write-file-hooks 'my-untabify-buffer)

(provide 'my-editing)
