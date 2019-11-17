;;; -*- lexical-binding: t; -*-

;;; Add my custom lisp dir to load-path
(defconst my-dir (expand-file-name "me" user-emacs-directory))
(defconst my-lisp-dir (expand-file-name "lisp" my-dir))
(defconst my-local-lisp-dir (expand-file-name "local" my-lisp-dir))

(add-to-list 'load-path my-lisp-dir)

;;; Harden security settings.
(require 'my-security)

;;; Trick emacs into not saving the customization file.
(setq custom-file "/dev/null" )

;;; Configure autoloads
;;; Regenerate using M-x update-directory-autoloads
(load (expand-file-name "autoloads.el" my-lisp-dir))

;;; This must come before configurations of installed packages. Emacs will
;;; insert this line if not present.
(package-initialize)

(require 'my-packages)
(require 'my-performance)
(require 'my-locale)
(require 'my-ui)
(require 'my-editing)
(with-eval-after-load 'minibuffer (require 'my-minibuffer))
(require 'my-vc)
(with-eval-after-load 'dired (require 'my-dired))
(with-eval-after-load 'comint (require 'my-shell))
(require 'my-tramp)
(require 'my-org)
(require 'my-text-mode)
(require 'my-prog-modes)
(require 'my-elisp)
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
