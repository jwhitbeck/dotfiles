;;; -*- lexical-binding: t; -*-

;;; Emacs UI Customizations

(require 'my-package)
(my-use-packages ace-jump-mode
                 ace-window
                 company
                 fill-column-indicator
                 fiplr
                 flx-ido
                 hide-lines
                 ido
                 ido-completing-read+
                 ido-vertical-mode
                 rainbow-delimiters
                 smex
                 zenburn-theme)

;;; Load Zen burn them
(load-theme 'zenburn t)

;;; Activate line highlighting everywhere
(global-hl-line-mode t)

;;; Automatically show opened/closed parentheses
(show-paren-mode)

;;; Show row/column number in model line
(column-number-mode t)
(line-number-mode t)

;;; Always answer yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; Disable useless UI elements
(when (and (fboundp 'menu-bar-mode) menu-bar-mode)
  (menu-bar-mode -1))                   ; disable menu bar
(when (and (fboundp 'tool-bar-mode) tool-bar-mode)
  (tool-bar-mode -1))                   ; disable tool bar
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode)
  (scroll-bar-mode -1))                 ; disable scroll bar

;;; Disable startup messages
(custom-set-variables
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message t)
 '(initial-scratch-message nil))

;;; Start scrolling the window when the cursor reaches its edge.
;;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(custom-set-variables
 '(scroll-margin 7)
 '(scroll-conservatively 10000)
 '(scroll-preserve-screen-position 1))

;;; Set M-- and M-+ to decrease/increase the font size
(defun my-text-scale-reset ()
  "Reset the height of the default face in the current buffer."
  (interactive)
  (text-scale-set 0))

(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-0") 'my-text-scale-reset)

;;; X clipboard <-> emacs kill ring compatibility
(custom-set-variables
 ;; stops selection with a mouse from being immediately injected to the kill ring
 '(mouse-drag-copy-region nil)
 ;; stops killing/yanking interacting with primary X11 selection
 '(x-select-enable-primary nil)
 ;; makes killing/yanking interact with clipboard X11 selection
 '(x-select-enable-clipboard t))

;;; Ace-jump bindings
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;; IMenu
(global-set-key (kbd "C-²") 'imenu)

;;; Window navigation
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-o") 'ace-window) ; Convenience binding for typing C-x o too quickly
(custom-set-variables '(aw-keys '(?q ?s ?d ?f ?h ?j ?k ?l)))

;;; TODO: Remove after I upgrade to emacs26. ace-window expects this function to be defined but it isn't in
;;; emacs25.
(defun frame-parent (_) nil)

;;; Winner mode saves the history of window splits
(winner-mode t)

;;; Splitting
(custom-set-variables
 '(split-height-threshold 110)
 '(split-width-threshold 220))

;;; Window management
(defun my-rotate-window-split-horizontal ()
  "Rotate windows from a 2-window vertical split to a 2-window horizontal split."
  (interactive)
  (when (and (= (count-windows) 2)
             ;; true if current split is horizontal
             (caar (window-tree)))
    (let* ((cur-win (selected-window))
           (other-win (next-window))
           (cur-buffer (window-buffer))
           (other-buffer (window-buffer other-win))
           (cur-edges (window-edges cur-win))
           (other-edges (window-edges other-win)))
      (delete-other-windows)
      (split-window-horizontally)
      ;; True if current buffer was initially below the other buffer.
      (when (> (cadr cur-edges) (cadr other-edges))
        (other-window 1))
      (set-window-buffer (selected-window) cur-buffer)
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'my-rotate-window-split-horizontal)

(defun my-rotate-window-split-vertical ()
  "Rotate windows from a 2-window horizontal split to a 2-window vertical split."
  (interactive)
  (when (and (= (count-windows) 2)
             ;; true if current split is horizontal
             (not (caar (window-tree))))
    (let* ((cur-win (selected-window))
           (other-win (next-window))
           (cur-buffer (window-buffer))
           (other-buffer (window-buffer other-win))
           (cur-edges (window-edges cur-win))
           (other-edges (window-edges other-win)))
      (delete-other-windows)
      (split-window-vertically)
      ;; True if current buffer was initially to the right of the other buffer.
      (when (> (car cur-edges) (car other-edges))
        (other-window 1))
      (set-window-buffer (selected-window) cur-buffer)
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x _") 'my-rotate-window-split-vertical)

(defun my-window-switch ()
  "Switch the buffers displayed in a 2-window split."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((cur-buffer (window-buffer))
           (other-buffer (window-buffer (next-window))))
      (set-window-buffer (next-window) cur-buffer)
      (set-window-buffer (selected-window) other-buffer)
      (select-window (next-window)))))

(with-eval-after-load 'server
  ;; server.el sets this keybinding, so overrule it after it loads.
  (global-set-key (kbd "C-x #") 'my-window-switch))
(global-set-key (kbd "C-x #") 'my-window-switch)

;;; Simplify window management for french keyboards
(defun my-split-window-below ()
  "Split the window vertically and move focus to the new window below the selected one."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun my-split-window-right ()
  "Split the window horizontal and move focus to the new window to the right of the selected one."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x à") 'delete-window) ; C-x 0
(global-set-key (kbd "C-x &") 'delete-other-windows) ; C-x 1
(global-set-key (kbd "C-x é") 'my-split-window-below) ; C-x 2
(global-set-key (kbd "C-x \"") 'my-split-window-right) ; C-x 3

;;; Lock buffers to windows
(defun my-set-buffer-name-face (face)
  (let ((cur (car mode-line-buffer-identification)))
    (setq-local mode-line-buffer-identification
                (list (propertize cur 'face face)))))

(defun my-toggle-dedicated-window ()
  "Toggle whether or not the window is dedicated to its buffer."
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p)))
  (my-set-buffer-name-face (if (window-dedicated-p) 'warning 'mode-line-buffer-id)))

(global-set-key (kbd "C-x ²") 'my-toggle-dedicated-window)

;;; Use Ubuntu's default font
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)

;;; Turn on syntax highlighting for all modes that support it.
(global-font-lock-mode t)

;;; Parentheses
(custom-set-variables
 '(show-paren-delay 0)                              ; immediately show matching parentheses
 '(show-paren-style 'expression))                   ; highlight full expression contained between parentheses
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; activate rainbow delimeters in prog mode

;;; Fill column indicator
(custom-set-variables
 '(fill-column 110))                    ; line wrap at 110 characters
(add-hook 'prog-mode-hook 'fci-mode)    ; show a bar beyond the fill-column

;;; Auto-completion
(global-company-mode)
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)

;; workaround for fill-column indicator
;; See https://github.com/company-mode/company-mode/issues/180
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest _)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest _)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;;; Remember recently visited files
(recentf-mode t)
(custom-set-variables
 '(recentf-max-saved-items 1000))

;;; Save your minibuffer history across Emacs sessions.
(savehist-mode t)

;;; IDO
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(ido-everywhere t)
(custom-set-variables
 '(ido-enable-flex-matching t)
 '(flx-ido-mode t)                      ; Use FLX matching engine
 '(ido-use-faces nil)                   ; disable ido faces to see flx highlights.
 '(ido-use-virtual-buffers t)
 '(ido-max-directory-size 100000))

;;; IDO for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; FIPLR: Fuzzy project file finding
(global-set-key (kbd "C-x p") 'fiplr-find-file)
(eval-when-compile
  (require 'fiplr))
(with-eval-after-load 'fiplr
  (custom-set-variables
   '(fiplr-ignored-globs '((directories (".git" ".svn" ".hg" ".bzr" ".deps" ".build" "target" "node_modules"))
                           (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf"
                                   "*.gz" "*.zip" ".DS_Store" "*.class" "*.pyc" "*.den"
                                   ".elc")))))

  ;; Drop an empty .fiplr_root file in a dir for fiplr to consider it a top-level dir
  (add-to-list 'fiplr-root-markers ".fiplr_root" t))

;;; Buffer lists
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(custom-set-variables
 '(uniquify-buffer-name-style 'forward))

;;; Don't wrap lines in grep-mode
(defun my-disable-line-wrap ()
  (setq truncate-lines t))
(add-hook 'grep-mode-hook 'my-disable-line-wrap)

;;; Miscellaneous
(custom-set-variables
 '(transient-mark-mode t)               ; highlight selection
 '(lazy-highlight-initial-delay 0)      ; immediately highlight all matches
 '(vc-follow-symlinks t)                ; follow symlinks for files under version control
 '(ring-bell-function 'ignore))         ; disable bell

(provide 'my-ui)
