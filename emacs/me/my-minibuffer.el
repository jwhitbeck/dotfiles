;;; -*- lexical-binding: t; -*-

;;; Minibuffer settings

(require 'paredit)

(defun my-minibuffer--conditionally-enable-paredit-mode ()
  "Enable paredit-mode during eval-expression."
  (when (eq this-command 'eval-expression)
    (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook
          'my-minibuffer--conditionally-enable-paredit-mode)

(provide 'my-minibuffer)
