;;; -*- lexical-binding: t; -*-

;;;; Whitespace helper functions
(require 'whitespace)

;;;###autoload
(defun my-whitespace-mode ()
  "Like whitespace-mode, but intelligently decides whether or not
to highlight tabs."
  (setq-local whitespace-style
              (if indent-tabs-mode
                  '(face empty trailing)
                '(face empty trailing tabs tab-mark)))
  (whitespace-mode 1))

(provide 'my-whitespace)
