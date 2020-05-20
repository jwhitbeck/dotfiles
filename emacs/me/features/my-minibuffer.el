;;; -*- lexical-binding: t; -*-

;;; Minibuffer settings

(declare-function paredit-mode "paredit")

(defun my-minibuffer--conditionally-enable-paredit-mode ()
  "Enable paredit-mode during eval-expression."
  (when (eq this-command 'eval-expression)
    (paredit-mode 1)))

(add-hook 'eval-expression-minibuffer-setup-hook
          'my-minibuffer--conditionally-enable-paredit-mode)

(provide 'my-minibuffer)
