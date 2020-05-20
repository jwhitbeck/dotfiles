;;; -*- lexical-binding: t; -*-

;;;; Remember recently visited files
(require 'recentf)

(recentf-mode 1)

(setq recentf-max-saved-items 1000)

(provide 'my-recentf)
