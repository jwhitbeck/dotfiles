;;; -*- lexical-binding: t; -*-

;;;; Settings for programming major modes

;;; Automatically show opened/closed parentheses
(require 'paren)
;;; show-paren is a global minor mode
(show-paren-mode)

(setq
 ;; Immediately show matching parentheses.
 show-paren-delay 0
 ;; Highlight full expression contained between parentheses.
 show-paren-style 'parenthesis)

;;; Activate rainbow delimeters in prog mode.
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Fill column indicator
(setq-default fill-column 80)		; Line wrap at 80 characters.
(add-hook 'prog-mode-hook 'fci-mode)	; Show a bar beyond the fill-column.
(with-eval-after-load 'fill-column-indicator
  (require 'my-fill-column-indicator))

;;; Enable Flycheck in all program editing modes
(add-hook 'prog-mode-hook 'flycheck-mode)
(with-eval-after-load 'flycheck
  ;; Enable files to opt-out of flycheck.
  (put 'flycheck-mode 'safe-local-variable 'booleanp)
  ;; Ensure that flycheck uses the customized emacs lisp load-path,
  ;; not the default one.
  ;; https://stackoverflow.com/questions/20498554/how-do-i-make-flycheck-find-required-file-in-emacs-lisp
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

;;; Activate undo tree for all program editing modes.
(add-hook 'prog-mode-hook 'undo-tree-mode)

;;; Activate flyspell for comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; No tabs by default. Modes that really need tabs should enable
;;; indent-tabs-mode explicitly. Makefile-mode already does that, for example.
(setq indent-tabs-mode nil)

;; Automatically revert a buffer when a file is changed on disk.
(add-hook 'prog-mode-hook 'auto-revert-mode)

;;; Remove the auto-indent hook that disables electric-indent
(with-eval-after-load 'auto-indent-mode
  (remove-hook 'after-change-major-mode-hook 'auto-indent-disable-electric))

;;; Per-language settings

(with-eval-after-load 'ess-mode (require 'my-ess-mode))
(with-eval-after-load 'make-mode (require 'my-make-mode))
(with-eval-after-load 'scheme (require 'my-scheme))
(with-eval-after-load 'go-mode (require 'my-go-mode))
(with-eval-after-load 'css-mode (require 'my-css-mode))
(with-eval-after-load 'sql (require 'my-sql))
(with-eval-after-load 'elisp-mode (require 'my-elisp-mode))
(with-eval-after-load 'java-mode (require 'my-java-mode))
(with-eval-after-load 'clojure-mode (require 'my-clojure-mode))

;;; Per-language org babel settings
(with-eval-after-load 'ob-clojure (require 'my-ob-clojure))

(provide 'my-prog-modes)
