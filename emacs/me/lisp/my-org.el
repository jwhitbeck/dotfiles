;;; -*- lexical-binding: t; -*-

;;;; Custom org-mode settings

(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'org-habit)

;;; General settings
(setq
 org-catch-invisible-edits 'error
 org-read-date-force-compatible-dates nil
 org-startup-indented t)

;;; Capture settings
(setq
 org-refile-use-outline-path t
 org-outline-path-complete-in-steps nil
 org-capture-use-agenda-date t)

;;; Babel settings
(setq
 org-src-fontify-natively t
 org-confirm-babel-evaluate nil)

;;; Load shell and emacs-lisp babel languages by default. The others should be
;;; enabled on a per org-file basis by executing a (require 'ob-X) code block.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;;; Agenda settings
(setq
 ;; Use day-month-year instead of month-day-year
 calendar-date-style 'european
 org-agenda-time-grid '((daily today require-timed remove-match)
                         (800 1000 1200 1400 1600 1800 2000)
                         "......" "----------------")
 ;; Display tags further to right in agenda buffers
 org-agenda-tags-column -100
 ;; Display tags further to right in org buffers
 org-tags-column -100
 ;; Cache agenda views
 org-agenda-sticky t
 ;; Only show the next occurrence of repeated events
 org-agenda-show-future-repeats nil
 org-agenda-todo-ignore-scheduled 'future
 org-stuck-projects '("+LEVEL=2+CATEGORY=\"Project\""
                       ("TODO" "WAITING")
                       nil
                       nil)
 ;; Show the full project tree in follow-mode
 org-agenda-follow-indirect t
 ;; Clear all default custom agenda views
 org-agenda-custom-commands nil
 org-agenda-dim-blocked-tasks 'invisible
 org-enforce-todo-dependencies t
 ;; Allow tags-todo to set ignore options
 org-agenda-tags-todo-honor-ignore-options t
 ;; Hide scheduled and deadline items in agenda that are marked as done
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t)

;;; Allow org-mode files to customize the org-archive-save-context-info variable.
(put 'org-archive-save-context-info 'safe-local-variable 'listp)

;;; Ace-jump-mode integration
(defun my-org-table--blank-field-or-jump ()
  "Like ace-jump but preserves C-c SPC behavior inside org-mode tables."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-blank-field)
    (call-interactively 'ace-jump-mode)))

(define-key org-mode-map (kbd "C-c SPC") 'my-org-table--blank-field-or-jump)

;;; Improve org-open-file types
(require 'my-external)
(dolist (kons my-external-programs)
  (let ((re (concat "\\." (car kons) "\\'"))
        (cmd (concat (cdr kons) " %s")))
    (add-to-list 'org-file-apps (cons re cmd))))

;;; Use org-tempo for the easy templates
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

;;; Adapted from `org-tempo--include-file'
(defun my-org-tempo--pgsql-properties ()
  "Inserts postgresql properties for running SQL from a babel code block."
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

(provide 'my-org)
