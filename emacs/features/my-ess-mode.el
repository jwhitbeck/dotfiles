;;; -*- lexical-binding: t; -*-

;;;; R via Emacs Speaks Statistics.
(require 'ess-mode)

(define-key ess-mode-map (kbd "_") 'ess-smarter-underscore)

(provide 'my-ess-mode)
