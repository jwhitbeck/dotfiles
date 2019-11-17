;;; -*- lexical-binding: t; -*-

;;;; CSS mode settings
(require 'css-mode)

(setq css-indent-offset 2)
(add-hook 'css-mode-hook 'my-whitespace-mode-default)

(provide 'my-css-mode)
