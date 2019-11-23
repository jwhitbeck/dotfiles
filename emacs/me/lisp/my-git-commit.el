;;; -*- lexical-binding: t; -*-

;;;; Git commit editing mode

(require 'git-commit)

;;; Follow this guide: https://chris.beams.io/posts/git-commit/

(setq git-commit-summary-max-length 50)

(defun my-git-commit-mode-fill-column ()
  (setq fill-column 72))

(add-hook 'git-commit-mode-hook 'my-git-commit-mode-fill-column)

(provide 'my-git-commit)
