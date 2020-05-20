;;; -*- lexical-binding: t; -*-

;;;; Geiser repl settings
(require 'geiser-repl)

(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'subword-mode)

(provide 'my-geiser-repl)
