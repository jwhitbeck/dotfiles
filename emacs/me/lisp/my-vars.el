;;; -*- lexical-binding: t; -*-

;;; Misc vars that don't fit anywhere else
(defvar my-pdf-reader
  (or (executable-find "evince")
      (executable-find "atril")))

(provide 'my-vars)
