;;; -*- lexical-binding: t; -*-

;;;; Powerline configuration

(require 'powerline)

;;; Enable powerline globally
(powerline-default-theme)

;;; Powerline customizations
(setq powerline-display-buffer-size nil
      powerline-display-hud nil)

(provide 'my-powerline)
