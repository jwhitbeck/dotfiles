;;; -*- lexical-binding: t; -*-

;;;; Scheme mode settings
(require 'geiser-impl)

(setq geiser-active-implementations '(chicken))
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)
(add-hook 'scheme-mode-hook 'subword-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'subword-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)
(add-hook 'scheme-mode-hook 'my-whitespace-mode-default)

(provide 'my-scheme)
