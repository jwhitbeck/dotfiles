;;; -*- lexical-binding: t; -*-

;;; Org settings that should be loaded after org-mode

(eval-when-compile
  (require 'ace-jump-mode)
  (require 'org-table))

;;; Ace-jump-mode integration
(defun org-table-blank-field-or-jump ()
  "Like ace-jump but preserves C-c SPC behavior inside org-mode tables."
  (interactive)
  (if (org-table-check-inside-data-field t)
      (call-interactively 'org-table-blank-field)
    (call-interactively 'ace-jump-mode)))
(define-key org-mode-map (kbd "C-c SPC") 'org-table-blank-field-or-jump)

;;; Improve org-open-file types
(add-to-list 'org-file-apps '("\\.maff\\'" . "firefox %s"))

;;; Enable the proselint org startup option
(require 'my-spellcheck)
(add-to-list 'org-startup-options '("proselint" my-use-proselint t) t)

(provide 'my-org-extras)
