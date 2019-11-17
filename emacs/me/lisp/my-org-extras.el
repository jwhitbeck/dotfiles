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
(add-to-list 'org-file-apps `("\\.pdf\\'" . ,(concat my-pdf-reader " %s")))

;;; Enable yasnippet in org-mode
;;; XXX remove
;; (require 'my-editing)
;; (add-hook 'org-mode-hook 'my-yas-minor-mode)

;;; Enable org-tempo for the easy templates
;;; https://orgmode.org/manual/Easy-templates.html
(require 'org-tempo)

(tempo-define-template "my-org-title"
                       '("#+TITLE: "
                         p >)
                       "<t"
                       "Insert title comment"
                       'org-tempo-tags)

(tempo-define-template "my-org-name"
                       '("#+NAME: "
                         p >)
                       "<n"
                       "Insert name comment"
                       'org-tempo-tags)

(defun my-org-tempo--pgsql-properties ()
  "XXX"
  (let ((inhibit-quit t))
    (unless (with-local-quit
              (prog1 t
                (insert ":PROPERTIES:\n"
                        (format ":header-args:sql: :engine postgresql :cmdline -U %s -h %s -F '|' %s\n"
                                (read-string "User name: ")
                                ;; XXX add a read-host helper
                                (read-string "Host: ")
                                (read-string "Database: "))
                        ":END:\n")))
      (insert "<pg")
      (setq quit-flag nil))))

(tempo-define-template "my-pgsql"
                       '((my-org-tempo--pgsql-properties)
                         p >)
                       "<pg"
                       "Insert PostgresSQL properties for querying a table."
                       'org-tempo-tags)

;;; Prevent C-c $ from overriding org-mode's archive-subtree binding
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-c $") nil))

;;; Prevent C-c ! from overriding org-mode's org-time-stamp-inactive
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c !") nil))


(provide 'my-org-extras)
