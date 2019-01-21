;;; -*- lexical-binding: t; -*-

;;; Custom org-mode setup

;;; org-plus-contrib is installed in my-package
(require 'my-package)
(require 'my-commands)

;;; Open all *.org files in org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;; Global keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)

;;; General settings
(custom-set-variables
 '(org-catch-invisible-edits 'error)
 '(org-read-date-force-compatible-dates nil)
 '(org-completion-use-ido t)
 '(org-startup-indented t))

;;; Capture settings
(custom-set-variables
 '(org-refile-use-outline-path t)
 '(org-outline-path-complete-in-steps nil)
 '(org-capture-use-agenda-date t))

;;; Babel settings
(custom-set-variables
 '(org-src-fontify-natively t)
 '(org-confirm-babel-evaluate nil)
 '(org-babel-load-languages '((clojure . t)
                              (emacs-lisp . t)
                              (R . t)
                              (sql . t)
                              (shell . t))))

;;; Agenda settings
(custom-set-variables
 ;; Use day-month-year instead of month-day-year
 '(calendar-date-style 'european)
 '(org-agenda-time-grid '((daily today require-timed remove-match)
                          (800 1000 1200 1400 1600 1800 2000)
                          "......" "----------------"))
 ;; Display tags further to right in agenda buffers
 '(org-agenda-tags-column -100)
 ;; Display tags further to right in org buffers
 '(org-tags-column -100)
 ;; Cache agenda views
 '(org-agenda-sticky t)
 ;; Only show the next occurrence of repeated events
 '(org-agenda-repeating-timestamp-show-all nil)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-stuck-projects '("+LEVEL=2+CATEGORY=\"Project\"" ("TODO" "WAITING") nil nil))
 ;; Show the full project tree in follow-mode
 '(org-agenda-follow-indirect t)
 ;; Clear all default custom agenda views
 '(org-agenda-custom-commands nil)
 '(org-agenda-dim-blocked-tasks 'invisible)
 '(org-enforce-todo-dependencies t)
 ;; Allow tags-todo to set ignore options
 '(org-agenda-tags-todo-honor-ignore-options t)
 ;; Hide scheduled and deadline items in agenda that are marked as done
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-if-done t))

;;; Allow org-mode files to customize the org-archive-save-context-info variable.
(put 'org-archive-save-context-info 'safe-local-variable 'listp)

;;; Load custom HTML export settings on demand
(with-eval-after-load 'ox-html
  (require 'my-org-html))

;;; Load org-mode extras
(with-eval-after-load 'org
  (require 'my-org-extras))

(with-eval-after-load 'org-agenda
  ;;; Use org-habit
  (require 'org-habit)
  ;;; Load appointment integration
  (require 'my-org-appt))

(provide 'my-org)
