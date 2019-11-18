;;; -*- lexical-binding: t; -*-

;;; General editing settings

;;; Don't use auto-backups.
(setq
 auto-save-default nil         ; Disable autosave.
 make-backup-files nil)        ; Disable auto backups.

;;; Always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; File associations.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;; Insert special characters using f7
(defvar my-f7-chars
  '(;; EM dash. See https://www.thepunctuationguide.com/em-dash.html for usage.
    ("_" . "—")
    ;; EN dash. See https://www.thepunctuationguide.com/en-dash.html for usage.
    ("-" . "–")))

(dolist (e my-f7-chars)
  (global-set-key (kbd (concat "<f7> " (car e)))
                  (lambda () (interactive) (insert (cdr e)))))

;;; Unfill paragraph
(global-set-key (kbd "M-Q") 'my-unfill-paragraph)

(provide 'my-editing)
