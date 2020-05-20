;;; -*- lexical-binding: t; -*-

;;;; Golang settings
(require 'go-mode)

(add-hook 'go-mode-hook 'my-tabs-enable-indent-tabs)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'my-whitespace-mode-tabs)

(setq gofmt-command "goimports")

(define-key go-mode-map (kbd "M-.") 'godef-jump)

(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'my-go-mode)
