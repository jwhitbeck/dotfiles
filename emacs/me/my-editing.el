;;; -*- lexical-binding: t; -*-

;;;; General editing settings

;;; Don't use auto-backups.
(setq
 auto-save-default nil         ; Disable autosave.
 make-backup-files nil)        ; Disable auto backups.

;;; Always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'my-editing)
