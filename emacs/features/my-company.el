;;; -*- lexical-binding: t; -*-

;;;; Auto-completion using company
(require 'company)

(global-company-mode)

;;; Hide minor mode indicator
(setq company-lighter nil)

(provide 'my-company)
