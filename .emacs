;;;; DOT EMACS

;;; TLS Ensure that certificates are verified. See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
;;; for full explanation.
(require 'gnutls)
(custom-set-variables '(gnutls-verify-error t))

;;; PACKAGES
(require 'package)
(custom-set-variables '(package-archives '(("org" . "http://orgmode.org/elpa/")
                                           ("gnu" . "https://elpa.gnu.org/packages/")
                                           ("melpa" . "https://melpa.org/packages/")
                                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                                           )))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(org-plus-contrib  ; This needs to come first
                      ace-jump-mode
                      ace-window
                      auto-indent-mode
                      browse-at-remote
                      cider
                      clojure-mode
                      coffee-mode
                      company
                      emacs-eclim
                      ess
                      ess-smart-underscore
                      fill-column-indicator
                      fiplr
                      flx-ido
                      flycheck
                      go-mode
                      gh-md
                      hide-lines
                      ido
                      ido-ubiquitous
                      ido-vertical-mode
                      magit
                      markdown-mode
                      paredit
                      paredit-menu
                      popup
                      rainbow-delimiters
                      slamhound
                      smex
                      undo-tree
                      yaml-mode
                      zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;; UI
(load-theme 'zenburn t)                 ; load zenburn theme
(global-hl-line-mode t)                 ; activate line highlighting everywhere
(show-paren-mode)                       ; Automatically show opened/closed parentheses
(column-number-mode t)                  ; show col number in bar
(line-number-mode t)                    ; show line number in bar
(fset 'yes-or-no-p 'y-or-n-p)           ; saner prompts
(custom-set-variables
 '(transient-mark-mode t)               ; highlight selection
 '(show-paren-delay 0)                  ; immediately show matching parentheses
 '(show-paren-style 'expression)        ; highlight full expression contained between parentheses
 '(inhibit-startup-message t)           ; disable startup message
 '(inhibit-startup-echo-area-message t)
 '(initial-scratch-message nil)         ; disable scratch message
 '(lazy-highlight-initial-delay 0)      ; immediately highlight all matches
 '(ring-bell-function 'ignore)          ; disable bell
 )

;;; Use UTF-8 as language environment
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(when (and (fboundp 'menu-bar-mode) menu-bar-mode) (menu-bar-mode -1))       ; disable menu bar
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))       ; disable tool bar
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1)) ; disable scroll bar

;;; Set M-- and M-+ to decrease/increase the font size
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))

;;; X clipboard <-> emacs kill ring compatibility
(custom-set-variables
 '(mouse-drag-copy-region nil)          ; stops selection with a mouse from being immediately injected to the
                                        ; kill ring
 '(x-select-enable-primary nil)         ; stops killing/yanking interacting with primary X11 selection
 '(x-select-enable-clipboard t))        ; makes killing/yanking interact with clipboard X11 selection

;;; Start scrolling the window when the cursor reaches its edge.
;;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(custom-set-variables
 '(scroll-margin 7)
 '(scroll-conservatively 10000)
 '(scroll-preserve-screen-position 1))

;;; Ace-jump bindings
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)


;;; PERFORMANCE
(customize-set-variable 'gc-cons-threshold 20000000) ; Reduce occurence of garbage collection
(custom-set-variables '(max-specpdl-size 2500)) ; increase limit of lisp variable bindings

;;; WINDOWS
;;; Splitting
(custom-set-variables
 '(split-height-threshold 80)
 '(split-width-threshold 220))
(defun split-window-sensibly-reverse (&optional window)
  "Identical to the built-in function split-window-sensibly, but prefers horizontal splits over
   vertical splits."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.(column-marker-1 80)
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it vertically disregarding
             ;; the value of `split-height-threshold'.
             (let ((split-height-threshold 0))
               (when (window-splittable-p window)
                 (with-selected-window window
                   (split-window-below))))))))
(customize-set-variable 'split-window-preferred-function 'split-window-sensibly-reverse)

;;; Window navigation
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-o") 'ace-window) ; Convenience binding for typing C-x o too quickly
(custom-set-variables '(aw-keys '(?q ?s ?d ?f ?h ?j ?k ?l)))

;;; Winner mode saves the history of window splits
(winner-mode t)

;;; Simplify window management for french keyboards
(global-set-key (kbd "C-x à") 'delete-window) ; C-x 0
(global-set-key (kbd "C-x &") 'delete-other-windows) ; C-x 1
(global-set-key (kbd "C-x é") (lambda () (interactive) (split-window-below) (other-window 1))) ; C-x 2
(global-set-key (kbd "C-x \"") (lambda () (interactive) (split-window-right) (other-window 1))) ; C-x 3
(global-set-key (kbd "C-x œ") 'winner-undo)


;;; FONTS
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)
(global-font-lock-mode t)


;;; EDITING
(custom-set-variables
 '(auto-save-default nil)               ; disable autosave
 '(make-backup-files nil)               ; disable auto backups
 '(vc-follow-symlinks t)                ; follow symlinks for files under version control
 '(fill-column 110)                     ; line wrap at 110 characters
 '(tab-width 2))                        ; distance between tab stops
(global-auto-revert-mode t)              ; automatically revert a buffer when a file is changed on disk
(global-undo-tree-mode t)                ; always activate undo tree
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; activate rainbow delimeters in prog mode
(add-hook 'text-mode-hook 'rainbow-delimiters-mode) ; activate rainbow delimeters in text mode
(add-hook 'prog-mode-hook 'fci-mode)                ; show a bar beyond the fill-column
(add-hook 'text-mode-hook 'fci-mode)                ; show a bar beyond the fill-column
(add-hook 'text-mode-hook 'flyspell-mode)           ; activate flyspell for all text modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)      ; activate flyspell for comments
(savehist-mode t)                       ; Save your minibuffer history across Emacs sessions. UX win!

;;; Highlight and auto-correct whitespace problems
(require 'whitespace)
(global-whitespace-mode t)
(custom-set-variables '(whitespace-style '(face empty trailing tabs tab-mark))
                      '(whitespace-global-modes '(not go-mode))) ; Disable for golang
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; No tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;;; Makefile-mode already does that, for example.
;;; If indent-tabs-mode is off, untabify before saving.
(customize-set-variable 'indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode) (untabify (point-min) (point-max)))
            nil))

;;; enable auto-complete
(require 'company)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(add-hook 'after-init-hook 'global-company-mode)
;; workaround for fill-column indicator
;; See https://github.com/company-mode/company-mode/issues/180
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
;; end of workaround

;;; MAGIT MODE
(require 'vc-git)            ; This needs to be required or else magit falls back to lgrep instead of git-grep

;;; IDO MODE
(require 'ido)
(require 'recentf)
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(ido-everywhere t)
(custom-set-variables
 '(ido-enable-flex-matching t)
 '(flx-ido-mode t)                      ; Use FLX matching engine
 '(ido-use-faces nil)                   ; disable ido faces to see flx highlights.
 '(ido-use-virtual-buffers t)
 '(ido-max-directory-size 100000)
 '(recentf-max-saved-items 1000))

;;; Ido for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;; FIPLR: Fuzzy project file finding
(require 'fiplr)
(customize-set-variable
 'fiplr-ignored-globs
 '((directories
    (".git" ".svn" ".hg" ".bzr" ".deps" "target"))
   (files
    (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" ".DS_Store" "*.class" "*.pyc"
     "*.den"))))
(global-set-key (kbd "C-x p") 'fiplr-find-file)
(add-to-list 'fiplr-root-markers ".fiplr_root" t)

;;; BUFFER LISTS
(require 'uniquify)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(customize-set-variable 'uniquify-buffer-name-style 'forward)


;;; DIRED
(require 'dired)
(require 'dired-x)
(require 'dired-aux)
(require 'dired-async)
;;; Enable disabled dired commands
(autoload 'dired-jump "dired-x" "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x" "Like \\[dired-jump] (dired-jump) but in other window." t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
(custom-set-variables '(dired-auto-revert-buffer t)     ; Auto-revert
                      '(dired-listing-switches "-alh")  ; Human-readable file sizes
                      '(dired-omit-files "^\\...+$")    ; Hide all dotfiles in dired-omit-mode
                      )
;;; Use async file operations
(dired-async-mode t)
;;; Activate dired-omit-mode by default
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode t)))
;;; Add binding to tail files in custom buffer
(defun tail-filename (filename &optional output-buffer-name)
  (interactive
   (let* ((filename (dired-get-filename t))
          (output-buffer-name (read-string (format "Output %s to buffer: " filename) nil
                                           nil (format "*Tail %s*" filename))))
     (list filename output-buffer-name)))
  (let ((command (concat "tail -n1000 -f " filename " &"))
        (handler (find-file-name-handler (directory-file-name default-directory) 'shell-command)))
    (if handler (apply handler 'shell-command (list command output-buffer-name))
      (shell-command command output-buffer-name)))
  (with-current-buffer output-buffer-name
    (setq truncate-lines t)))
(define-key dired-mode-map (kbd "C-c t") 'tail-filename)
;;; Better default programs for opening files
(custom-set-variables '(dired-guess-shell-alist-user
                        '(("\\.e?ps\\'" "evince")
                          ("\\.mpe?g\\'\\|\\.avi\\'" "vlc")
                          ("\\.ogg\\'" "vlc")
                          ("\\.mp3\\'" "vlc")
                          ("\\.m4a\\'" "vlc")
                          ("\\.wav\\'" "vlc")
                          ("\\.p[bgpn]m\\'" "eog")
                          ("\\.gif\\'" "eog")
                          ("\\.tif\\'" "eog")
                          ("\\.png\\'" "eog")
                          ("\\.jpe?g\\'" "eog")
                          ("\\.pdf\\'" "evince")
                          ("\\.doc\\'" "libreoffice")
                          ("\\.odt\\'" "libreoffice")
                          ("\\.mobi\\'" "ebook-viewer")
                          ("\\.epub\\'" "ebook-viewer")
                          ("\\.html\\'" "firefox")
                          ("\\.maff\\'" "firefox"))))
;;; Detached command-on-file execution
(defun dired-run-detached-shell-command (command)
  (let ((handler
         (find-file-name-handler (directory-file-name default-directory)
                                 'shell-command)))
    (if handler (apply handler 'detached-shell-command (list command))
      (detached-shell-command command)))
  ;; Return nil for sake of nconc in dired-bunch-files.
  nil)

(defun dired-do-detached-shell-command (command &optional arg file-list)
  "Like dired-do-shell-command, but detaches the processes."
  ;;Functions dired-run-shell-command and dired-shell-stuff-it do the
  ;;actual work and can be redefined for customization.
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "§ on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (let* ((on-each (not (string-match-p dired-star-subst-regexp command)))
         (no-subst (not (string-match-p dired-quark-subst-regexp command)))
         (star (string-match-p "\\*" command))
         (qmark (string-match-p "\\?" command)))
    ;; Get confirmation for wildcards that may have been meant
    ;; to control substitution of a file name or the file name list.
    (if (cond ((not (or on-each no-subst))
               (error "You can not combine `*' and `?' substitution marks"))
              ((and star on-each)
               (y-or-n-p "Confirm--do you mean to use `*' as a wildcard? "))
              ((and qmark no-subst)
               (y-or-n-p "Confirm--do you mean to use `?' as a wildcard? "))
              (t))
        (if on-each
            (dired-bunch-files
             (- 10000 (length command))
             (function (lambda (&rest files)
                         (dolist (file files)
                           (dired-run-detached-shell-command
                            (concat command dired-mark-separator (shell-quote-argument file))))))
             nil
             file-list)
          ;; execute the shell command
          (dired-run-detached-shell-command
           (dired-shell-stuff-it command file-list nil arg))))))

(defun dired-do-xdg-open (&optional arg file-list)
  "Wrapper around dired-do-detached-shell-command that always uses the xdg-open command."
  (interactive
   (list current-prefix-arg
         (dired-get-marked-files t current-prefix-arg)))
  (dired-do-detached-shell-command "xdg-open" current-prefix-arg file-list))

(define-key dired-mode-map (kbd "§") 'dired-do-detached-shell-command)
(define-key dired-mode-map (kbd "œ") 'dired-do-xdg-open)

;;; NOTIFICATIONS

;;; Adapted from http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
;;; Assumes that emacs is running on ubuntu
(defun my-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the
title of the message, MSG is the context. Optionally, you can
provide an ICON and SOUND."
  (interactive)
  (when sound
    (call-process "pacmd" nil 0 nil "play-file" sound "0"))
  (if (eq window-system 'x)
      (let ((args (if icon (list "-i" icon title msg) (list title msg))))
        (apply 'call-process "notify-send" nil 0 nil args))
    ;; text only version
    (message (format "%s: %s" title msg))))


;;; ORG MODE
(require 'org)
(require 'org-table)
(require 'ob-clojure)
(require 'ob-R)
(require 'ob-sh)
(require 'ob-sql)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(custom-set-variables '(org-catch-invisible-edits 'error)
                      '(org-read-date-force-compatible-dates nil)
                      '(org-completion-use-ido t)
                      '(org-src-fontify-natively t)
                      '(org-confirm-babel-evaluate nil)
                      '(org-babel-load-languages '((clojure . t)
                                                   (emacs-lisp . t)
                                                   (R . t)
                                                   (sql . t)
                                                   (sh . t)))
                      '(calendar-date-style 'european) ; Use day-month-year instead of month-day-year
                      '(org-agenda-time-grid '((daily today require-timed remove-match)
                                               #("----------------" 0 16
                                                 (org-heading t))
                                               (800 1000 1200 1400 1600 1800 2000)))
                      '(org-agenda-tags-column -100)  ; Display tags further to right in agenda buffers
                      '(org-tags-column -100) ; Display tags further to right in org buffers
                      '(org-agenda-sticky t) ; Cache agenda views
                      '(org-agenda-todo-ignore-scheduled 'future)
                      '(org-stuck-projects '("+LEVEL=2+CATEGORY=\"Project\"" ("TODO" "WAITING") nil ""))
                      '(org-agenda-follow-indirect t) ; Show the full project tree in follow-mode
                      '(org-agenda-custom-commands nil)
                      '(org-agenda-dim-blocked-tasks 'invisible)
                      '(org-enforce-todo-dependencies t))

;;; Org HTML export

;;; Online resources
;;; - https://emacs.stackexchange.com/questions/7629/the-syntax-highlight-and-indentation-of-source-code-block-in-exported-html-file
;;; - https://vxlabs.com/2015/01/28/sending-emails-with-math-and-source-code/
(defconst my-css-dir (concat user-emacs-directory "me/css/"))
(defconst my-js-dir (concat user-emacs-directory "me/js/"))

(defun my-build-html-head ()
  "Returns a string of inline css to use in org-mode's html export."
  (let ((css-files (mapcar (lambda (file) (concat my-css-dir file))
                           '("bootstrap.min.css"
                             "bootstrap.diff.css"
                             "code.css"
                             "org-html-export.css"))))
    (with-temp-buffer
      (insert "<style type=\"text/css\">\n")
      (dolist (file css-files)
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "\n"))
      (insert "</style>\n")
      (buffer-string))))

(defun my-build-html-scripts ()
  "Returns a string of line javascript for use in org-modes's html export."
  (with-temp-buffer
    (insert "<script type=\"text/javascript\">\n")
    (insert-file-contents (concat my-js-dir "org.js"))
    (goto-char (point-max))
    (insert "</script>\n")
    (buffer-string)))

(defconst my-org-html-postamble
  (concat "<dl class=\"dl-horizontal\">\n"
          "  <dt>Author</dt><dd>%a</dd>\n"
          "  <dt>Updated</dt><dd>%C</dd>\n"
          "</dl>\n"))

(custom-set-variables
 '(org-html-doctype "html5")
 '(org-html-html5-fancy t)
 '(org-html-head-include-default-style nil)
 '(org-html-postamble t)
 '(org-html-postamble-format `(("en" ,my-org-html-postamble)))
 '(org-html-scripts (my-build-html-scripts))
 '(org-html-head (my-build-html-head))
 '(org-html-htmlize-output-type 'css)
 '(org-html-htmlize-font-prefix "org-"))

;;; Fix issue where fill-column-indicator outputs unprintable characters at the end of code blocks
;;; https://github.com/alpaker/Fill-Column-Indicator/issues/45
(defun fci-mode-override-advice (&rest args))
(advice-add 'org-html-fontify-code :around
            (lambda (fun &rest args)
              (advice-add 'fci-mode :override #'fci-mode-override-advice)
              (let ((result  (apply fun args)))
                (advice-remove 'fci-mode #'fci-mode-override-advice)
                result)))

;;; Improve org-open-file types
(add-to-list 'org-file-apps '("\\.maff\\'" . "firefox %s"))

;;; Ace-jump-mode integration
(defun org-table-blank-field-or-jump ()
  (interactive)
  (if (org-table-check-inside-data-field t)
      (call-interactively 'org-table-blank-field)
    (call-interactively 'ace-jump-mode)))
(define-key org-mode-map (kbd "C-c SPC") 'org-table-blank-field-or-jump)


;;; appointments integration
(require 'appt)
(appt-activate 't)

(defun my-update-appointments-on-agenda-save ()
  (when (member (buffer-file-name) org-agenda-files)
    (org-agenda-to-appt 't)))
(add-hook 'after-save-hook 'my-update-appointments-on-agenda-save)

(defun my-appt-notify (time-to-appt time msg)
  (my-popup (format "Appointment in %s minutes" time-to-appt)
            msg
            "/usr/share/icons/gnome/256x256/status/appointment-soon.png"
            "/usr/share/sounds/ubuntu/stereo/message.ogg"))

(custom-set-variables '(appt-display-interval (+ 1 appt-message-warning-time)) ; no duplicate reminders
                      '(app-display-mode-line nil) ; disable mode-line reminders
                      '(appt-disp-window-function 'my-appt-notify))

;;; Holidays
(defvar holiday-french-holidays
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

;; FLYCHECK
(require 'flycheck)
(require 'flyspell)
(global-flycheck-mode t)
(customize-set-variable 'flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
;;; Prevent C-c $ binding in flyspell-mode as it overrides the org-mode binding
(define-key flyspell-mode-map (kbd "C-c $") nil)

;;; proselint integration, see http://unconj.ca/blog/linting-prose-in-emacs.html
;;; Only turn on proselint in org-mode buffers if #+STARTUP: proselint is set.
(defvar my-org-use-proselint nil "If t, will turn on proselint in the org-mode buffer.")
(add-to-list 'org-startup-options '("proselint" my-org-use-proselint t) t)

(when (executable-find "proselint")
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :predicate (lambda ()
                 (and (or (not ispell-current-dictionary)
                          ;; only use when typing in english
                          (string-prefix-p "en" ispell-current-dictionary))
                      (or my-org-use-proselint
                          (member major-mode '(text-mode markdown-mode gfm-mode mu4e-compose-mode))))))
  (add-to-list 'flycheck-checkers 'proselint))

;;; MARKDOWN
(add-hook 'markdown-mode-hook 'orgtbl-mode)

;;; CLOJURE
(require 'cider)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;;; Org-babel
(custom-set-variables '(org-babel-clojure-backend 'cider))

;;; Clojure indentation rules
(define-clojure-indent
  (send-off 1)                                   ; Core
  (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2)  ; Compojure
  (select 1) (insert 1) (update 1) (delete 1)    ; Korma
  (where 1) (set-fields 1) (values 1) (upsert 1) ; Korma (continued)
  (clone-for 1)                                  ; Enlive
  (up 1) (down 1) (alter 1) (table 1) (create 1) ; Lobos
  (fact 2) (facts 2)                             ; Midje
  )

(custom-set-variables
 ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established.
 '(cider-repl-pop-to-buffer-on-connect nil)
 ;; Hide the cider process buffers
 '(nrepl-hide-special-buffers t)
 ;; Don't log all nrepl protocol messages-buffer
 '(nrepl-log-messages nil)
 ;; Auto-focus the error buffer when it's displayed after evaluating some clojure code. This makes it easy to
 ;; type "q" to dismiss the window, assuming you don't want this backtrace window hanging around.
 '(cider-auto-select-error-buffer t))

;;; Fix indentation for single semicolon comments
(defun lisp-indent-line-single-semicolon-fix (&optional whole-exp)
  "Identical to the built-in function lisp-indent-line,
   but doesn't treat single semicolons as right-hand-side comments."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
        ;; Don't alter indentation of a ;;; comment line
        ;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
        (goto-char (- (point-max) pos))
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))
(add-hook 'clojure-mode-hook (lambda () (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)))

;;; Prevent C-c SPC binding in clojure mode as it overrides the ace-window-mode key binding
(define-key clojure-mode-map (kbd "C-c SPC") nil)


;;; JAVA
;;; For compatibility with Google style guide
(c-add-style "java-google" '("user" (c-offsets-alist . ((case-label . +)
                                                        (arglist-intro . ++)
                                                        (statement-cont . ++)))))
(add-hook 'java-mode-hook (lambda ()
                            (set-fill-column 100)
                            (c-set-style "java-google")))


;;; GREP MODE
;;; don't wrap lines in grep mode
(add-hook 'grep-mode-hook (lambda () (customize-set-variable 'truncate-lines t)))
(global-set-key (kbd "C-x g") 'vc-git-grep)

;;; EMACS LISP
(add-hook 'emacs-lisp-mode-hook 'paredit-mode) ; enable paredit
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; ELISP MINIBUFFER
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (when (eq this-command 'eval-expression)
    (paredit-mode 1)))

;;; FILE EXTENSIONS
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))


;;; TRAMP
;;; sudo on remote servers
(let ((local-host-regex (concat "\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\|" system-name "\\)")))
  (customize-set-variable 'tramp-default-proxies-alist
                          `((,local-host-regex "root" nil) (".*" "root" "/ssh:%h:"))))
(custom-set-variables
 '(remote-file-name-inhibit-cache nil) ; always use the remote file-name cache for read access
 '(tramp-completion-reread-directory-timeout nil)  ; always use the cache for remote completion
 '(vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" ; Disable version control checks on remote files
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))
 )
;;; Always add $HOME/bin to the remote path
(add-to-list 'tramp-remote-path "~/bin")
;;; command for quickly opening shells on remote hosts.
(defun remote-shell-at-point ()
  "Opens a remote shell on the host-name under point."
  (interactive)
  (let ((hostname (thing-at-point 'filename)))
    (let ((buffer-name (format "*%s*" hostname))
          (default-directory (format "/%s:" hostname))
          (current-prefix-arg '-))      ; C-u
      (shell buffer-name))))

;;; TERM
(require 'term)
;;; Bind smex in the character mode
(define-key term-raw-map (kbd "C-c M-x") 'smex)
(define-key term-raw-map (kbd "C-c M-X") 'smex-major-mode-commands)
;;; Not all remote servers can handle the default 'eterm-color'
(setq term-term-name "ansi")
;;; workaround for ansi-term
(eval-after-load 'zenburn
  '(setq ansi-term-color-vector
         [unspecified "#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"]))


;;; SHELL
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)    ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t)   ; scroll to show max possible output
 '(comint-completion-autolist t)          ; show completion list when ambiguous
 '(comint-input-ignoredups t)             ; no duplicates in command history
 '(comint-completion-addsuffix t)         ; insert space/slash after file completion
 '(comint-get-old-input (lambda () ""))   ; what gets sent to prompt when pressing enter in the buffer
 '(comint-buffer-maximum-size 20000)      ; max length of buffer in lines
 '(comint-input-ring-size 5000)           ; max shell history size
 '(explicit-shell-file-name "/bin/bash")) ; Always use bash on remote hosts
(setenv "PAGER" "cat")                                             ; Do not use `less` as the default pager
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; truncate buffers continuously

;;; Run commands in the background and output to a specific buffer
(defvar-local refreshable-shell-command--command nil)

(defun refreshable-shell-command--refresh ()
  (interactive)
  (when-let ((cmd refreshable-shell-command--command))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if-let ((handler (find-file-name-handler (directory-file-name default-directory) 'shell-command)))
          (funcall handler 'shell-command cmd t nil)
        (call-process shell-file-name nil t nil shell-command-switch cmd)))))

(defun refreshable-shell-command (command buffer-or-name)
  "Run command synchronously and send output to the provided
buffer. The buffer will be marked read-only. Press 'g' to refresh
the output of the command. Press 'q' to dismiss the buffer."
  (interactive
   (let ((cmd (read-shell-command "Shell command: " nil nil))
         (buffer-name (read-string "Output to buffer: ")))
     (list cmd buffer-name)))
  (let ((buf (get-buffer-create buffer-or-name))
        (directory default-directory))
    (with-current-buffer buf
      (read-only-mode)
      (setq default-directory directory)
      (local-set-key (kbd "g") 'refreshable-shell-command--refresh)
      (local-set-key (kbd "q") 'kill-this-buffer)
      (setq refreshable-shell-command--command command)
      (refreshable-shell-command--refresh))
    (set-window-buffer nil buf)))

;;; Run commands in a detached process
(defun detached-shell-command (command)
  "Like shell-command but runs the shell command in a process detached from emacs."
  (interactive
   (list (read-shell-command "Detached shell command: " nil nil
                             (let ((filename
                                    (cond
                                     (buffer-file-name)
                                     ((eq major-mode 'dired-mode)
                                      (dired-get-filename nil t)))))
                               (and filename (file-relative-name filename))))))
  (let ((handler
         (find-file-name-handler (directory-file-name default-directory)
                                 'shell-command)))
    (if handler
        (funcall handler 'detached-shell-command command)
      (call-process shell-file-name nil 0 nil shell-command-switch command))))

(global-set-key (kbd "M-§") 'detached-shell-command)


;;; SQL
;;; Add option to chose port in sql-postgres and use localhost as default server.
(require 'sql)
(customize-set-variable
 'sql-postgres-login-params
 `((user :default ,(user-login-name))
   (database :default ,(user-login-name))
   (server :default "localhost")
   (port :default 5432)))

;;; CSS
(require 'css-mode)
(customize-set-variable 'css-indent-offset 2)

;;; YAML
;; Add a few extra minor-modes since yaml-mode does not derive from either prog-mode or text-mode
(add-hook 'yaml-mode-hook 'fci-mode) ; show bar beyond the fill column
(add-hook 'yaml-mode-hook 'flyspell-mode) ; turn on automatic spell-checking

;;; Eclispe integration
(require 'eclim)
(require 'eclimd)

(global-eclim-mode)
(eval-when-compile (require 'cl)) ; company-emacs-eclim requires 'cl to be required for the remove-if function
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

;;; Emacs Speaks Statistis
(require 'ess)
(require 'ess-smart-underscore)
(add-hook 'ess-mode-hook (lambda () (local-key-binding (kbd "_") 'ess-smarter-underscore)))

;;; Easy PG
(require 'epg)
(custom-set-variables '(epg-gpg-program "gpg2")) ; Default to GnuPG >2.0 on ubuntu

;;; MISC
(defun current-utc-time ()
  "Returns an ISO 8061 UTC timestamp"
  (interactive)
  (set-time-zone-rule t)
  (let ((timestamp (prog1 (format-time-string "%Y-%m-%d-T%TZ")
                     (set-time-zone-rule nil))))
    (if (called-interactively-p 'interactive)
        (message "%s" timestamp)
      timestamp)))

(global-set-key (kbd "C-c u") 'current-utc-time)

;;; Load machine-specific config
(let ((file "~/.emacs.d/config.el"))
  (when (file-exists-p file)
    (load-file file)))
