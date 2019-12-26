;;; -*- lexical-binding: t; -*-

;;;; Markdown mode settings

(require 'markdown-mode)
(require 'org-table)

(defun my-enable-orgtbl-mode ()
  (orgtbl-mode 1))

(add-hook 'markdown-mode-hook 'my-enable-orgtbl-mode)
(add-hook 'markdown-mode-hook 'fci-mode)
(add-hook 'markdown-mode-hook 'my-whitespace-mode-default)

(provide 'my-markdown-mode)
