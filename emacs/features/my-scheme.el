;;; -*- lexical-binding: t; -*-

;;;; Scheme mode settings
(require 'scheme)

(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)
(add-hook 'scheme-mode-hook 'subword-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'subword-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)
(add-hook 'scheme-mode-hook 'my-whitespace-mode)

(provide 'my-scheme)
