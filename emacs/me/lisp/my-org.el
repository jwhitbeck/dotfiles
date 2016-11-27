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
 '(org-completion-use-ido t))

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
                          #("----------------" 0 16
                            (org-heading t))
                          (800 1000 1200 1400 1600 1800 2000)))
 ;; Display tags further to right in agenda buffers
 '(org-agenda-tags-column -100)
 ;; Display tags further to right in org buffers
 '(org-tags-column -100)
 ;; Cache agenda views
 '(org-agenda-sticky t)
 ;; Only show the next occurrence of repeated events
 '(org-agenda-repeating-timestamp-show-all nil)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-stuck-projects '("+LEVEL=2+CATEGORY=\"Project\"" ("TODO" "WAITING") ("appt") ""))
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

;;; Holidays
(defconst holiday-french-holidays
  `((holiday-fixed 1 1 "Jour de l'an")
    (holiday-fixed 1 6 "Épiphanie")
    (holiday-fixed 2 2 "Chandeleur")
    (holiday-fixed 2 14 "Saint Valentin")
    (holiday-fixed 5 1 "Fête du travail")
    (holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
    (holiday-fixed 6 21 "Fête de la musique")
    (holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
    (holiday-fixed 8 15 "Assomption (Religieux)")
    (holiday-fixed 11 11 "Armistice de 1918")
    (holiday-fixed 11 1 "Toussaint")
    (holiday-fixed 11 2 "Commémoration des fidèles défunts")
    (holiday-fixed 12 25 "Noël")
    ;; fêtes à date variable
    (holiday-easter-etc 0 "Pâques")
    (holiday-easter-etc 1 "Lundi de Pâques")
    (holiday-easter-etc 39 "Ascension")
    (holiday-easter-etc 49 "Pentecôte")
    (holiday-easter-etc -47 "Mardi gras")
    ;; dernier dimanche de mai ou premier dimanche de juin si c'est le
    ;; même jour que la pentecôte TODO
    (holiday-float 5 0 -1 "Fête des mères")
    ;; troisième dimanche de juin
    (holiday-float 6 0 3 "Fête des pères"))
  "French holidays")

(custom-set-variables '(holiday-bahai-holidays nil) ; Disable bahai holidays
                      '(holiday-other-holidays holiday-french-holidays))


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
