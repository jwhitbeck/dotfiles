;;; -*- lexical-binding: t; -*-

;;;; Auto-completion using company
(require 'company)
(require 'my-mode-line)

(global-company-mode)
(my-mode-line-hide-minor-mode 'company-mode)

(provide 'my-company)
