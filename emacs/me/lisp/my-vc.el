;;; -*- lexical-binding: t; -*-

;;; Version control

(require 'my-package)
(my-use-packages browse-at-remote magit)

(custom-set-variables '(magit-delete-by-moving-to-trash nil))

;;; Bind git-grep
(defun my-git-grep ()
  (interactive)
  (require 'vc-git)
  (call-interactively 'vc-git-grep))

(global-set-key (kbd "C-x g") 'my-git-grep)

(provide 'my-vc)
