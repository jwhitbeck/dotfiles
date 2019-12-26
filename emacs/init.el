;;; -*- lexical-binding: t; -*-

;;; Add my custom lisp dir to load-path
(require 'my-dirs (expand-file-name "me/lisp/my-dirs.el" user-emacs-directory))
(add-to-list 'load-path my-lisp-dir)

;;; Harden security settings.
(require 'my-security)

;;; Keep the Emacs customization system out of init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; This must come before configurations of installed packages. Emacs will
;;; insert this line if not present.
(package-initialize)

;;; Load theme. Must come first because it re-loads custom.el.
(require 'my-packages)
(unless (package-installed-p 'zenburn-theme)
  (my-packages-install 'zenburn-theme))
(load-theme 'zenburn t)

;;; Install packages on startup
(dolist (pkg my-packages)
  (my-packages-install pkg))

;;; Configure autoloads
;;; Regenerate using M-x update-directory-autoloads
(load (expand-file-name "autoloads.el" my-lisp-dir))

;;; Read autoloads for advanced dired functions.
(load "dired-loaddefs.el")

(require 'my-performance)
(require 'my-locale)
(require 'my-global-keybindings)
(require 'my-file-associations)
(require 'my-mode-line)
(require 'my-ui)
(require 'my-editing)
(require 'my-ivy)
(require 'my-projectile)
(with-eval-after-load 'minibuffer (require 'my-minibuffer))
(with-eval-after-load 'git-commit (require 'my-git-commit))
(with-eval-after-load 'flyspell (require 'my-flyspell))
(with-eval-after-load 'magit (require 'my-magit))
(with-eval-after-load 'dired (require 'my-dired))
(with-eval-after-load 'comint (require 'my-shell))
(with-eval-after-load 'tramp (require 'my-tramp))
(with-eval-after-load 'org (require 'my-org))
(with-eval-after-load 'ox (require 'my-ox))
(with-eval-after-load 'nov (require 'my-nov))
(with-eval-after-load 'ace-window (require 'my-ace-window))
(require 'my-text-mode)
(require 'my-prog-modes)

;;; Start emacs server
(server-start)

;;; Load local init.
(when (file-symlink-p my-local-lisp-dir)
  (add-to-list 'load-path my-local-lisp-dir)
  (load (expand-file-name "init.el" my-local-lisp-dir)))
