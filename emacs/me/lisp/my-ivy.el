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

(my-hide-minor-mode 'ivy-mode)
(my-hide-minor-mode 'counsel-mode)

;;; Default to `ivy' matching, but use `fuzzy' matching (via flx) for some
;;; callers (i.e., the value in `this-command').
(setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        (counsel-describe-function . ivy--regex-fuzzy)
        (counsel-describe-variable . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

(setq
 ;; add recent files and/or bookmarks to ‘ivy-switch-buffer’.
 ivy-use-virtual-buffers t
 ;; Don't propose ../ when listing files.
 ivy-extra-directories '("./"))

(provide 'my-ivy)
