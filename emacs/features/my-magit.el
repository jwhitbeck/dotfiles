;;; -*- lexical-binding: t; -*-

;;;; Magit settings
(require 'magit)

(setq magit-delete-by-moving-to-trash nil)

;;; Don't let magit rebind C-x g, which I bind to vc-git-grep.
(define-key magit-file-mode-map (kbd "C-x g") nil)

(provide 'my-magit)
