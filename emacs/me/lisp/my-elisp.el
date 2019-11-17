;;; -*- lexical-binding: t; -*-

;;; Emacs lisp settings

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode 'my-whitespace-mode-default)

;;; minibuffer
(defun my-conditionally-enable-paredit-mode ()
  "Enable paredit-mode during eval-expression."
  (when (eq this-command 'eval-expression)
    (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'my-conditionally-enable-paredit-mode)

;;; Disable emacs-lisp-checkdoc flycheck because it creates at lot of noise in
;;; my personal emacs config files
(require 'flycheck)
(custom-set-variables
 `(flycheck-checkers ,(delq 'emacs-lisp-checkdoc flycheck-checkers)))

(provide 'my-elisp)
