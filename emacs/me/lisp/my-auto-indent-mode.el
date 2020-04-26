;;; -*- lexical-binding: t; -*-

;;; Auto-indent mode settings

;;; Hide minor mode
(require 'my-mode-line)
(my-mode-line-hide-minor-mode 'auto-indent-mode)

;;; Remove the auto-indent hook that disables electric-indent
(with-eval-after-load 'auto-indent-mode
  (remove-hook 'after-change-major-mode-hook 'auto-indent-disable-electric))

(provide 'my-auto-indent-mode)
