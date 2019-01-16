;;; -*- lexical-binding: t; -*-

;;; Org settings that should be loaded after org-mode

(eval-when-compile
  (require 'ace-jump-mode)
  (require 'org-table))

(require 'my-vars)

;;; Ace-jump-mode integration
(defun my-org-table-blank-field-or-jump ()
  "Like ace-jump but preserves C-c SPC behavior inside org-mode tables."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-blank-field)
    (call-interactively 'ace-jump-mode)))
(define-key org-mode-map (kbd "C-c SPC") 'my-org-table-blank-field-or-jump)

;;; Improve org-open-file types
(add-to-list 'org-file-apps '("\\.zhtml\\'" . "zhtml-open %s"))
(add-to-list 'org-file-apps `("\\.pdf\\'" . ,(concat pdf-reader " %s")))

;;; Enable the proselint org startup option
(require 'my-spellcheck)

(defvar my-org-use-proselint nil
  "If t, enable proselint in org-mode buffer.")

(defun my-org-enable-proselint ()
  (setq my-use-proselint my-org-use-proselint))

(my-enable-proselint-for-mode 'org-mode)
(add-hook 'org-mode-hook 'my-org-enable-proselint)
(add-to-list 'org-startup-options '("proselint" my-org-use-proselint t) t)

;;; Enable yasnippet in org-mode
(require 'my-editing)
(add-hook 'org-mode-hook 'my-yas-minor-mode)

;;; Enable org-tempo for the easy templates
;;; https://orgmode.org/manual/Easy-templates.html
(require 'org-tempo)

(provide 'my-org-extras)
