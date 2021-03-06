;;; -*- lexical-binding: t; -*-

;;;; Markdown mode settings

(require 'markdown-mode)
(require 'org-table)

(defun my-markdown--enable-orgtbl-mode ()
  (orgtbl-mode 1))

(add-hook 'markdown-mode-hook 'my-markdown--enable-orgtbl-mode)
(add-hook 'markdown-mode-hook 'fci-mode)
(add-hook 'markdown-mode-hook 'my-whitespace-mode)

(provide 'my-markdown-mode)
