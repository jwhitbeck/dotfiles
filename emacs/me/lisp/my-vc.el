;;; -*- lexical-binding: t; -*-

;;;; Version control settings

(with-eval-after-load 'magit
  (require 'my-magit))

(global-set-key (kbd "C-x g") 'vc-git-grep)

(provide 'my-vc)
