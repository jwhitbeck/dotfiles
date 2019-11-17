;;; -*- lexical-binding: t; -*-

;;;; Locations for custom settings

(defconst my-dir (expand-file-name "me" user-emacs-directory))
(defconst my-lisp-dir (expand-file-name "lisp" my-dir))
(defconst my-local-lisp-dir (expand-file-name "local" my-lisp-dir))

(provide 'my-dirs)
