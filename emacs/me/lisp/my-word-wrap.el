;;; -*- lexical-binding: t; -*-

;;;; Cycle between word-wrapping settings
(require 'visual-fill-column)

(defvar-local my-word-wrap-state 'no-wrap
  "One of no-wrap, wrap-window, or wrap-fill-column.")

;;;###autoload
(defun my-word-wrap ()
  (interactive)
  (cl-case my-word-wrap-state
    ;; Switch to wrap-window
    ('no-wrap (progn (toggle-truncate-lines 0)
                     (toggle-word-wrap 1)
                     (visual-fill-column-mode 0)
                     (setq my-word-wrap-state 'wrap-window)
                     (message "Wrap text at window edge.")))
    ;; Switch to wrap-fill-column
    ('wrap-window (progn (toggle-truncate-lines 0)
                         (toggle-word-wrap 1)
                         (visual-fill-column-mode 1)
                         (setq my-word-wrap-state 'wrap-fill-column)
                         (message "Wrap text at fill column.")))
    ;; Switch to long lines
    ('wrap-fill-column (progn (toggle-truncate-lines 1)
                              (toggle-word-wrap 0)
                              (visual-fill-column-mode 0)
                              (setq my-word-wrap-state 'no-wrap)
                              (message "Text wrapping disabled.")))))

(provide 'my-word-wrap)
