;;; -*- lexical-binding: t; -*-

;;; Auto-indent mode settings
(require 'auto-indent-mode)

;;; Hide minor mode
(require 'my-mode-line)
(my-mode-line-hide-minor-mode 'auto-indent-mode)

;;; Remove the auto-indent hook that disables electric-indent
(remove-hook 'after-change-major-mode-hook 'auto-indent-disable-electric)

(provide 'my-auto-indent-mode)
