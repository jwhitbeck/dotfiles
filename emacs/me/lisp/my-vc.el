;;; -*- lexical-binding: t; -*-

;;; Version control

(require 'my-package)
(my-use-packages browse-at-remote magit)

(custom-set-variables
 '(magit-delete-by-moving-to-trash nil)
 '(git-commit-fill-column 70))

;;; Git grep
(autoload 'vc-git-grep "vc-git")

;;; Quote default pattern to prevent shell expansion of wildcards
(defun my-quote-grep-read-files (pattern)
  (concat "\"" pattern "\""))

(advice-add 'grep-read-files :filter-return 'my-quote-grep-read-files)

;;; Remap C-x g to vc-git-grep
(with-eval-after-load 'magit
  (define-key magit-file-mode-map (kbd "C-x g") nil))
(global-set-key (kbd "C-x g") 'vc-git-grep)

(provide 'my-vc)
