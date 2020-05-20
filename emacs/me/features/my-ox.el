;;; -*- lexical-binding: t; -*-

;;;; Org-export settings
(require 'ox)
(require 'ox-gfm)

;;; Disable table of content by default. Re-enable on a file-by-file basis by
;;; adding #+OPTIONS: toc:t
(setq org-export-with-toc nil)

(provide 'my-ox)
