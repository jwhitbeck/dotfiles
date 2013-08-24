;;; PACKAGES
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(defvar my-packages '(ac-nrepl
                      auto-complete
                      auto-indent-mode
                      coffee-mode
                      clojure-mode
                      clojure-test-mode
                      fiplr
                      fill-column-indicator
                      flycheck
                      hide-lines
                      ido
                      ido-ubiquitous
                      ido-vertical-mode
                      git-commit-mode
                      magit
                      markdown-mode
                      midje-mode
                      nrepl
                      paredit
                      paredit-menu
                      rainbow-delimiters
                      smart-tab
                      smex
                      switch-window
                      undo-tree
                      zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;; UI
(load-theme 'zenburn t) ; load zenburn theme
(eval-after-load 'zenburn '(setq ansi-term-color-vector [unspecified "#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])) ; workaround for ansi-term
(setq transient-mark-mode t) ; hilight selection
(global-hl-line-mode t) ; activate line highlighting everywhere
(show-paren-mode) ; Automatically show opened/closed parentheses
(eval-after-load 'paren '(setq show-paren-delay 0)) ; immediately show matching parentheses
(eval-after-load 'paren '(setq show-paren-style 'expression)) ; highlight full expression contained between parentheses
(setq inhibit-startup-message t) ; disable startup message
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil) ; disable scratch message
(setq lazy-highlight-initial-delay 0) ; immediately highlight all matches
(setq ring-bell-function 'ignore) ; disable bell
(when (and (fboundp 'menu-bar-mode) menu-bar-mode) (menu-bar-mode -1)) ; disable menu bar
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1)) ; disable tool bar
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1)) ; disable scroll bar
(column-number-mode t) ; show col number in bar
(line-number-mode t) ; show line number in bar

;; X clipboard <-> emacs kill ring compatibility
(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;;; WINDOWS
;; Splitting
(setq split-height-threshold 80)
(setq split-width-threshold 220)
(setq split-window-preferred-function 'split-window-sensibly-reverse)
(defun split-window-sensibly-reverse (&optional window)
  "Identical to the built-in function split-window-sensibly, but prefers horizontal splits over vertical splits."
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

;; Window navigation
(require 'switch-window)
(windmove-default-keybindings)

;; Simplify window management for french keyboards
(global-set-key (kbd "C-x à") (global-key-binding (kbd "C-x 0")))
(global-set-key (kbd "C-x &") (global-key-binding (kbd "C-x 1")))
(global-set-key (kbd "C-x é") (global-key-binding (kbd "C-x 2")))
(global-set-key (kbd "C-x \"") (global-key-binding (kbd "C-x 3")))


;;; FONTS
(set-face-attribute 'default nil :family "Monospace" :height 100)
(global-font-lock-mode t)


;;; EDITING
(require 'undo-tree)
(require 'rainbow-delimiters)
(setq auto-save-default nil) ; disable autosave
(setq make-backup-files nil) ; disable auto backups
(setq vc-follow-symlinks t) ; follow symlinks for files under version control
(set-keyboard-coding-system 'mule-utf-8) ; default to utf-8
(global-auto-revert-mode t) ; automatically revert a buffer when a file is changed on disk
(global-undo-tree-mode t) ; always activate undo tree
(setq-default tab-width 2) ; distance between tab stops
(global-rainbow-delimiters-mode t) ; activate rainbow delimeters everywhere
(custom-set-variables '(fill-column 110)) ; line wrap at 110 characters
(add-hook 'prog-mode-hook 'fci-mode) ; show a bar beyond the fill-column
(add-hook 'text-mode-hook 'fci-mode) ; show a bar beyond the fill-column
(add-hook 'text-mode-hook 'turn-on-flyspell) ; activate flyspell for all text modes
(savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

;; Highlight and auto-correct whitespace problems
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     (setq whitespace-style '(face empty trailing tabs tab-mark))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;; Makefile-mode already does that, for example.
;; If indent-tabs-mode is off, untabify before saving.
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
            nil))

;; Smart tabs
(require 'smart-tab)
(global-smart-tab-mode t)

;; enable auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(ac-linum-workaround)

;; Automatic syntax checking
(require 'flycheck)
(global-flycheck-mode t)
(eval-after-load 'flycheck
  '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))



;;; IDO MODE
(require 'ido)
(require 'ido-vertical-mode)
(require 'smex)
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(eval-after-load 'ido
  '(progn
     (setq ido-enable-flex-matching t)
     (setq ido-use-virtual-buffers t)
     (setq ido-everywhere t)
     (setq ido-max-directory-size 100000)
     (setq recentf-max-saved-items 1000)))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; Fuzzy project file finding
(require 'fiplr)
(setq fiplr-ignored-globs
      '((directories
         (".git" ".svn" ".hg" ".bzr"))
        (files
         (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" ".DS_Store"))))
(global-set-key (kbd "C-x p") 'fiplr-find-file)

;;; BUFFER LISTS
(require 'uniquify)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq uniquify-buffer-name-style 'forward) ; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)


;;; DIRED
;; Enable disabled dired commands
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
;; Auto-revert
(eval-after-load 'dired '(setq dired-auto-revert-buffer t))


;;; ORG MODE
(require 'org-install)
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)


;;; CLOJURE
(require 'clojure-mode)
(require 'nrepl)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
;; Clojure indentation rules
(define-clojure-indent
  (send-off 1)                                  ; Core
  (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) ; Compojure
  (select 1) (insert 1) (update 1) (delete 1)   ; Korma
  (clone-for 1)                                 ; Enlive
  (up 1) (down 1)                               ; Lobos
  (fact 2) (facts 2)                            ; Midje
  )
;; Autocompletion in nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load 'auto-complete '(add-to-list 'ac-modes 'nrepl-mode))

;;; GREP MODE
(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-grep)

;;; EMACS LISP
(add-hook 'emacs-lisp-mode-hook 'paredit-mode) ; enable paredit
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)

;;; FILE EXTENSIONS
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))


;;; TRAMP
;; sudo on remote servers
(set-default 'tramp-default-proxies-alist (quote (("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\|xps\\)\\'" "\\`root\\'" nil)
                                                  (".*" "\\`root\\'" "/ssh:%h:"))))

;;; SQL
;; Add option to chose port in sql-postgres
(require 'sql)
(add-to-list 'sql-postgres-login-params '(port :default 5432) t)
