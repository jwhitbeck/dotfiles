;;; -*- lexical-binding: t; -*-

;;;; Emacs UI settings
(require 'my-mode-line)

;;; Use Ubuntu's default font
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)

;;; Turn on syntax highlighting for all modes that support it.
(global-font-lock-mode 1)

;;; Activate line highlighting everywhere
(global-hl-line-mode 1)

;;; Show row/column number in model line
(column-number-mode 1)
(line-number-mode 1)

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
(setq
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message nil)

;;; Suppress warnings
(require 'warnings)
;;; Don't warn if undo info exceeds `undo-outer-limit'.
(add-to-list 'warning-suppress-types '(undo discard-info))

;;; Start scrolling the window when the cursor reaches its edge.
;;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq
 scroll-margin 7
 scroll-conservatively 10000
 scroll-preserve-screen-position 1)

;;; X clipboard <-> emacs kill ring compatibility
(require 'mouse)
;; stops selection with a mouse from being immediately injected to the kill
;; ring
(setq mouse-drag-copy-region nil)

(require 'select)
(setq
 ;; stops killing/yanking interacting with primary X11 selection
 select-enable-primary nil
 ;; makes killing/yanking interact with clipboard X11 selection
 select-enable-clipboard t)

;;; Winner mode saves the history of window
;;; Don't bind winner-mode keys as we are going to define our own.
(require 'winner)
(setq winner-dont-bind-my-keys t)
(winner-mode 1)

;;; Window splitting
(setq
 split-height-threshold 110
 split-width-threshold 220)

;;; Auto-completion
(require 'company)
(global-company-mode)
(my-mode-line-hide-minor-mode 'company-mode)

;;; Remember recently visited files
(recentf-mode 1)
(require 'recentf)
(setq recentf-max-saved-items 1000)

;;; Save your minibuffer history across Emacs sessions.
(savehist-mode 1)

;;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Don't wrap lines in grep-mode
(defun my-ui--disable-line-wrap ()
  (setq truncate-lines t))
(add-hook 'grep-mode-hook 'my-ui--disable-line-wrap)

;;; Miscellaneous
(setq
 transient-mark-mode t               ; Highlight selection.
 lazy-highlight-initial-delay 0      ; Immediately highlight all matches.
 ring-bell-function 'ignore          ; Disable bell.
 vc-follow-symlinks t)        ; Follow symlinks for files under version control.

(provide 'my-ui)