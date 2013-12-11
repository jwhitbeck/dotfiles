;;; PACKAGES
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(defvar my-packages '(ac-nrepl
                      auto-complete
                      auto-indent-mode
                      cider
                      cider-tracing
                      coffee-mode
                      clojure-mode
                      clojure-test-mode
                      fiplr
                      fill-column-indicator
                      flycheck
                      flx-ido
                      hide-lines
                      ido
                      ido-ubiquitous
                      ido-vertical-mode
                      git-commit-mode
                      magit
                      markdown-mode
                      midje-mode
                      paredit
                      paredit-menu
                      rainbow-delimiters
                      smart-tab
                      smex
                      switch-window
                      undo-tree
                      yaml-mode
                      zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;; UI
(require 'paren)
(load-theme 'zenburn t) ; load zenburn theme
(eval-after-load 'zenburn '(setq ansi-term-color-vector [unspecified "#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])) ; workaround for ansi-term
(setq transient-mark-mode t) ; hilight selection
(global-hl-line-mode t) ; activate line highlighting everywhere
(show-paren-mode) ; Automatically show opened/closed parentheses
(setq show-paren-delay 0) ; immediately show matching parentheses
(setq show-paren-style 'expression) ; highlight full expression contained between parentheses
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
(fset 'yes-or-no-p 'y-or-n-p) ; saner prompts

;; Set M-- and M-+ to decrease/increase the font size
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))

;; X clipboard <-> emacs kill ring compatibility
(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;;; PERFORMANCE
(setq gc-cons-threshold 20000000) ;; Reduce occurence of garbage collection


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
(global-set-key (kbd "C-x o") 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
(windmove-default-keybindings)

;; Winner mode
;; Winner mode saves the history of window splits
(winner-mode t)

;; Simplify window management for french keyboards
(global-set-key (kbd "C-x à") 'delete-window) ; C-x 0
(global-set-key (kbd "C-x &") 'delete-other-windows) ; C-x 1
(global-set-key (kbd "C-x é") (lambda () (interactive) (split-window-below) (other-window 1))) ; C-x 2
(global-set-key (kbd "C-x \"") (lambda () (interactive) (split-window-right) (other-window 1))) ; C-x 3
(global-set-key (kbd "C-x œ") 'winner-undo)


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
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-style '(face empty trailing tabs tab-mark))
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
(add-hook 'prog-mode-hook 'smart-tab-mode)
(add-hook 'sql-interactive-mode-hook 'smart-tab-mode)

;; enable auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(ac-linum-workaround)

;; Automatic syntax checking
(require 'flycheck)
(global-flycheck-mode t)
(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))


;;; MAGIT MODE
(require 'vc-git) ; This needs to be required or else magit falls back to lgrep instead of git-grep
(require 'magit)


;;; IDO MODE
(require 'ido)
(require 'ido-vertical-mode)
(require 'smex)
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 100000)
(setq recentf-max-saved-items 1000)
(setq flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(ido-everywhere t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;; FIPLR: Fuzzy project file finding
(require 'fiplr)
(setq fiplr-ignored-globs
      '((directories
         (".git" ".svn" ".hg" ".bzr"))
        (files
         (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" ".DS_Store" "*.class"))))
(global-set-key (kbd "C-x p") 'fiplr-find-file)
(add-hook 'magit-checkout-command-hook (lambda (_) (fiplr-clear-cache) nil)) ; Invalidate fiplr cache

;;; BUFFER LISTS
(require 'uniquify)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq uniquify-buffer-name-style 'forward) ; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)


;;; DIRED
(require 'dired)
;; Enable disabled dired commands
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
;; Auto-revert
(setq dired-auto-revert-buffer t)
;; Human-readable file sizes
(setq dired-listing-switches "-alh")
;; Add binding to tail files in custom buffer
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

;;; ORG MODE
(require 'org-install)
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)


;;; CLOJURE
(require 'clojure-mode)
(require 'cider)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'clojure-mode-hook 'midje-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Clojure indentation rules
(define-clojure-indent
  (send-off 1)                                   ; Core
  (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2)  ; Compojure
  (select 1) (insert 1) (update 1) (delete 1)    ; Korma
  (where 1) (set-fields 1) (values 1) (upsert 1) ; Korma (continued)
  (clone-for 1)                                  ; Enlive
  (up 1) (down 1) (alter 1) (table 1) (create 1) ; Lobos
  (fact 2) (facts 2)                             ; Midje
  )

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established.
(setq cider-repl-pop-to-buffer-on-connect nil)
;; Auto-focus the error buffer when it's displayed after evaluating some clojure code. This makes it easy
;; to type "q" to dismiss the window, assuming you don't want this backtrace window hanging around.
(setq cider-auto-select-error-buffer t)
;; Autocompletion in cider
(require 'auto-complete)
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'cider-mode)
;; Fix indentation for single semicolon comments
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
(add-hook 'clojure-mode-hook '(lambda () (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)))


;;; GREP MODE
(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))
(global-set-key (kbd "C-x g") 'magit-grep)

;;; EMACS LISP
(add-hook 'emacs-lisp-mode-hook 'paredit-mode) ; enable paredit
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

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
;; use ssh as default method (use this to share ControlMaster between tramp and shells)
(setq tramp-default-method "ssh")


;;; TERM
;; Bind smex in the character mode
(require 'term)
(define-key term-raw-map (kbd "C-c M-x") 'smex)
(define-key term-raw-map (kbd "C-c M-X") 'smex-major-mode-commands)
;; Not all remote servers can handle the default 'eterm-color'
(setq term-term-name "ansi")


;;; SHELL
(setq comint-scroll-to-bottom-on-input t) ; always insert at the bottom
(setq comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
(setq comint-scroll-show-maximum-output t) ; scroll to show max possible output
(setq comint-completion-autolist t) ; show completion list when ambiguous
(setq comint-input-ignoredups t) ; no duplicates in command history
(setq comint-completion-addsuffix t) ; insert space/slash after file completion
(setq comint-get-old-input (lambda () "")) ; what gets sent to prompt when pressing enter in the buffer
(setq comint-buffer-maximum-size 20000) ; max length of buffer in lines
(setq comint-input-ring-size 5000) ; max shell history size
(setenv "PAGER" "cat") ; Do not use `less` as the default pager
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; truncate buffers continuously
(setq explicit-shell-file-name "/bin/bash") ; Always use bash on remote hosts


;;; SQL
;; Add option to chose port in sql-postgres and use localhost as default server.
(require 'sql)
(setq sql-postgres-login-params `((user :default ,(user-login-name))
                                  (database :default ,(user-login-name))
                                  (server :default "localhost")
                                  (port :default 5432)))

;;; CSS
(set-variable 'css-indent-offset 2)

;;; YAML
;; Add a few extra minor-modes since yaml-mode does not dervice from either prog-mode or text-mode
(add-hook 'yaml-mode-hook 'fci-mode) ; show bar beyond the fill column
(add-hook 'yaml-mode-hook 'flyspell-mode) ; turn on automatic spell-checking
