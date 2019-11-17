;;; -*- lexical-binding: t; -*-

;;;; Emacs UI settings

;;; Load Zen burn them
(load-theme 'zenburn t)

;;; Use Ubuntu's default font
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)

;;; Activate line highlighting everywhere
(global-hl-line-mode t)

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
(setq
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message nil)

;;; Start scrolling the window when the cursor reaches its edge.
;;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq
 scroll-margin 7
 scroll-conservatively 10000
 scroll-preserve-screen-position 1)

;;; Set M-- and M-+ to decrease/increase the font size
(defun my-text-scale-reset ()
  "Reset the height of the default face in the current buffer."
  (interactive)
  (text-scale-set 0))

(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-0") 'my-text-scale-reset)

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

;;; Ace-jump bindings
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;; IMenu
(global-set-key (kbd "C-²") 'imenu)

;;; Window navigation
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-o") 'ace-window) ; Convenience binding for typing C-x o too quickly
(with-eval-after-load 'ace-window
  (require 'my-ace-window))

;;; Winner mode saves the history of window
;;; Don't bind winner-mode keys as we are going to define our own.
(require 'winner)
(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "<f8>") 'winner-undo)
(global-set-key (kbd "<f9>") 'winner-redo)

;;; Window splitting
(setq
 split-height-threshold 110
 split-width-threshold 220)

;;; Window management
(global-set-key (kbd "C-x |") 'my-rotate-window-split-horizontal)
(global-set-key (kbd "C-x _") 'my-rotate-window-split-vertical)
(global-set-key (kbd "C-x #") 'my-window-switch)
(with-eval-after-load 'server
  ;; server.el sets this keybinding, so overrule it after it loads.
  (global-set-key (kbd "C-x #") 'my-window-switch))

;;; Simplify window management for french keyboards
(global-set-key (kbd "C-x à") 'delete-window) ; C-x 0
(global-set-key (kbd "C-x &") 'delete-other-windows) ; C-x 1
(global-set-key (kbd "C-x é") 'my-split-window-below) ; C-x 2
(global-set-key (kbd "C-x \"") 'my-split-window-right) ; C-x 3
(global-set-key (kbd "C-x ²") 'my-toggle-dedicated-window)

;;; Auto-completion
(require 'company)
(global-company-mode)
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)

;;; Remember recently visited files
(recentf-mode t)
(require 'recentf)
(setq recentf-max-saved-items 1000)

;;; Save your minibuffer history across Emacs sessions.
(savehist-mode t)

;;; IDO
(require 'ido)
(require 'flx-ido)
(require 'ido-completing-read+)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(ido-everywhere t)
(setq
 ido-enable-flex-matching t
 flx-ido-mode t                       ; Use FLX matching engine.
 ido-use-faces nil                    ; Disable ido faces to see flx highlights.
 ido-use-virtual-buffers t
 ido-max-directory-size 100000)

;;; IDO for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; FIPLR: Fuzzy project file finding
(with-eval-after-load 'fiplr
  (require 'my-fiplr))
(global-set-key (kbd "C-x p") 'fiplr-find-file)

;;; Buffer lists
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Don't wrap lines in grep-mode
(defun my-disable-line-wrap ()
  (setq truncate-lines t))
(add-hook 'grep-mode-hook 'my-disable-line-wrap)

;;; Print current UTC time in echo area
(global-set-key (kbd "C-c u") 'my-current-utc-time)

;;; Quickly connect to remote hosts
(global-set-key (kbd "C-c s") 'my-remote-shell)

;;; Miscellaneous
(setq
 transient-mark-mode t               ; Highlight selection.
 lazy-highlight-initial-delay 0      ; Immediately highlight all matches.
 ring-bell-function 'ignore          ; Disable bell.
 vc-follow-symlinks t)        ; Follow symlinks for files under version control.


(provide 'my-ui)
