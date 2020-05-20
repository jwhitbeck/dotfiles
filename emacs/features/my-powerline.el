;;; -*- lexical-binding: t; -*-

;;;; Powerline configuration

(require 'powerline)

;;; Enable powerline globally
(powerline-default-theme)

;;; Powerline customizations
(setq powerline-display-buffer-size nil
      powerline-display-hud nil)

;;; Clear these faces to use the default powerline faces instead
(setq face-remapping-alist
      '((org-mode-line-clock)))

(provide 'my-powerline)
