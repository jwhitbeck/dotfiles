;;; -*- lexical-binding: t; -*-

;;; Version control

(require 'my-package)
(my-use-packages browse-at-remote magit)

(custom-set-variables '(magit-delete-by-moving-to-trash nil))

;;; Bind git-grep
(autoload 'vc-git-grep "vc-git")

(global-set-key (kbd "C-x g") 'vc-git-grep)

(provide 'my-vc)
