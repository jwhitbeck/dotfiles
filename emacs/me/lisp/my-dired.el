;;; -*- lexical-binding: t; -*-

;;; Dired customizations

(require 'my-package)
(my-use-packages async peep-dired)

;;; Add some extra global key bindings
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer."
  t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window."
  t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;;; Defer loading most extra dired settings until after after dired itself is
;;; loaded.
(defun my-load-dired-extras ()
  (require 'my-dired-extras))
(add-hook 'dired-load-hook 'my-load-dired-extras)

(provide 'my-dired)
