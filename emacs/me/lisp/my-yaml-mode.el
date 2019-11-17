;;; -*- lexical-binding: t; -*-

;;;; YAML settings

;; yaml-mode inherits from text mode, so fill-column indicator must be loaded
;; explicitly.
(add-hook 'yaml-mode-hook 'fci-mode)
(add-hook 'css-mode-hook 'my-whitespace-mode-default)

(provide 'my-yaml-mode)
