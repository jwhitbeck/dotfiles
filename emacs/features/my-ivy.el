;;; -*- lexical-binding: t; -*-

;;;; IVY, counsel, and swiper settings
;;;
;;; https://github.com/abo-abo/swiper

(require 'ivy)
(require 'counsel)
(require 'my-mode-line)

;;; Enable ivy globally
(ivy-mode 1)
(counsel-mode 1)

(my-mode-line-hide-minor-mode 'ivy-mode)
(my-mode-line-hide-minor-mode 'counsel-mode)

(setq
 ;; add recent files and/or bookmarks to ‘ivy-switch-buffer’.
 ivy-use-virtual-buffers t
 ;; Don't propose ../ when listing files.
 ivy-extra-directories '("./"))

(provide 'my-ivy)
