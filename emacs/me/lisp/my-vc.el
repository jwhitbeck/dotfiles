;;; -*- lexical-binding: t; -*-

;;; Version control

(require 'my-package)
(my-use-packages browse-at-remote magit)

(custom-set-variables
 '(magit-delete-by-moving-to-trash nil)
 '(git-commit-fill-column 70)
 ;; added from my-ui
 '(vc-follow-symlinks t)                ; follow symlinks for files under version control
 )

;;; Git grep
(autoload 'vc-git-grep "vc-git")

;;; Remap C-x g to vc-git-grep
(with-eval-after-load 'magit
  (define-key magit-file-mode-map (kbd "C-x g") nil))
(global-set-key (kbd "C-x g") 'vc-git-grep)

(provide 'my-vc)
