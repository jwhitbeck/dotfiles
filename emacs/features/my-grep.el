;;; -*- lexical-binding: t; -*-

;;;; Grep-mode settings

(require 'grep)

;;; Don't wrap lines in grep-mode
(defun my-ui--disable-line-wrap ()
  (setq truncate-lines t))

(add-hook 'grep-mode-hook 'my-ui--disable-line-wrap)

(provide 'my-grep)
