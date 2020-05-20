;;; -*- lexical-binding: t; -*-

;;;; YAML settings
(require 'yaml-mode)

;; yaml-mode inherits from text mode, so fill-column indicator must be loaded
;; explicitly.
(add-hook 'yaml-mode-hook 'fci-mode)

(provide 'my-yaml-mode)
