;;; -*- lexical-binding: t; -*-

;;;; Locations for custom settings

;;; These must be defined in a separate feature to avoid compilation warnings in
;;; other features that requires these directories.

(defconst my-dir (expand-file-name "me" user-emacs-directory)
  "Directory containing custom Emacs Lisp code.")

(add-to-list 'load-path my-dir)

(defconst my-features-dir (expand-file-name "features" my-dir)
  "Directory containing lazy-loaded settings for features.")

(add-to-list 'load-path my-features-dir)

(defconst my-local-dir (expand-file-name "local" my-dir)
  "Directory containing custom Emacs List init code for the local machine.")

(when (file-symlink-p my-local-dir)
  (add-to-list 'load-path my-local-dir))

(provide 'my-dirs)
