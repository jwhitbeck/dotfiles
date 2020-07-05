;;; -*- lexical-binding: t; -*-

;;;; Auto-completion using company
(require 'company)

;;; This is the setup recommended by the package authors.
(add-hook 'after-init-hook 'global-company-mode)

;;; Consistent key bindings with ivy
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-f") 'company-filter-candidates)

;;; Hide minor mode indicator
(setq company-lighter nil)

(provide 'my-company)
