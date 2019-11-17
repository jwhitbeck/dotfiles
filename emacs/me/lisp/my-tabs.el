;;; -*- lexical-binding: t; -*-

;;;; Helper functions for handling tabs.

;;;###autoload
(defun my-untabify-buffer ()
  "Replace all tabs with spaces in buffer."
  (unless indent-tabs-mode
    (untabify (point-min) (point-max)))
  nil)

;;;###autoload
(defun my-enable-indent-tabs ()
  "Enable using tabs for indentation. Intended for use in hooks."
  (setq indent-tabs-mode t))
