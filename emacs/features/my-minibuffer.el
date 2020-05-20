;;; -*- lexical-binding: t; -*-

;;; Minibuffer settings

(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'subword-mode)

(provide 'my-minibuffer)
