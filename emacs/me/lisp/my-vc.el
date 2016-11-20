;;; -*- lexical-binding: t; -*-

;;; Version control

(require 'my-package)
(my-use-packages browse-at-remote magit)

(custom-set-variables '(magit-delete-by-moving-to-trash nil))

;;; Git grep
(autoload 'vc-git-grep "vc-git")

;;; Quote default pattern to prevent shell expansion of wildcards
(defun my-quote-grep-read-files (pattern)
  (concat "\"" pattern "\""))

(advice-add 'grep-read-files :filter-return 'my-quote-grep-read-files)

(global-set-key (kbd "C-x g") 'vc-git-grep)

(provide 'my-vc)
