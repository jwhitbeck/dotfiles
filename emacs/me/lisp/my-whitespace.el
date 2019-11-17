;;; -*- lexical-binding: t; -*-

;;;; Whitespace helper functions
(require 'whitespace)

(defun my-whitespace-mode--enable (style)
  (setq-local whitespace-style style)
  (whitespace-mode))

;;;###autoload
(defun my-whitespace-mode-default ()
  (my-whitespace-mode--enable '(face empty trailing tabs tab-mark)))

;;;###autoload
(defun my-whitespace-mode-tabs ()
  (my-whitespace-mode--enable '(face empty trailing)))

(provide 'my-whitespace)
