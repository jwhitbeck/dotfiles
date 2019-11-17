;;; -*- lexical-binding: t; -*-

;;;; Preferred external programs

(defconst my-pdf-reader
  (or (executable-find "evince")
      (executable-find "atril")))

(defvar my-external-programs
  `(("pdf" . ,my-pdf-reader)
    ("epub" . ,my-pdf-reader)
    ("mobi" . ,my-pdf-reader)
    ("zhtml" . "zhtml-open")))

(provide 'my-external)
