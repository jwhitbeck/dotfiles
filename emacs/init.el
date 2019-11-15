;;; -*- lexical-binding: t; -*-

;;; Add my custom lisp dir to load-path
(defconst my-dir (expand-file-name "me" user-emacs-directory))
(defconst my-lisp-dir (expand-file-name "lisp" my-dir))
(defconst my-local-lisp-dir (expand-file-name "local" my-lisp-dir))

(add-to-list 'load-path my-lisp-dir)

;; This must come before configurations of installed packages. Emacs will insert this line if not present.
(package-initialize)
;;; Harden security settings.
(require 'my-security)

;;; Trick emacs into not saving the customization file as I already have these defined in my init files.
(setq custom-file "/dev/null" )

(require 'my-performance)
(require 'my-locale)
(require 'my-ui)
(require 'my-editing)
(require 'my-elisp)
(require 'my-spellcheck)
(require 'my-vc)
(require 'my-commands)
(require 'my-dired)
(require 'my-shell)
(require 'my-tramp)
(require 'my-org)
(require 'my-prog-modes)
(require 'my-java)
(require 'my-clojure)
(require 'my-email)
(require 'my-wiki)
(require 'my-rest)
(require 'my-publish)

;;; Start emacs server
(server-start)

(when (file-symlink-p my-local-lisp-dir)
  (add-to-list 'load-path my-local-lisp-dir)
  (load (expand-file-name "init.el" my-local-lisp-dir)))
