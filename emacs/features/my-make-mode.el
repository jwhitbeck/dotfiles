;;; -*- lexical-binding: t; -*-

;;;; Makefile mode settings
(require 'make-mode)

(add-hook 'makefile-gmake-mode-hook 'my-whitespace-mode)

(provide 'my-make-mode)
