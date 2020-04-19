;;; -*- lexical-binding: t; -*-

;;;; Mode line configuration

;;; Enable powerline
(require 'powerline)
(powerline-default-theme)

;;; Powerline customizations
(setq powerline-display-buffer-size nil
      powerline-display-hud nil)

(defun my-mode-line-hide-minor-mode (mode)
  "Hide the minor mode from the mode line. Useful to hide
  globally enabled minor modes."
  (let ((kons (assq mode minor-mode-alist)))
    (setcdr kons '(nil))))

(provide 'my-mode-line)
