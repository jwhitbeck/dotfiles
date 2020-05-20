;;; -*- lexical-binding: t; -*-

;;; Add custom lisp dirs to load-path
(require 'my-dirs (expand-file-name "me/my-dirs.el" user-emacs-directory))

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
(load (expand-file-name "autoloads.el" my-dir))

;;; Read autoloads for advanced dired functions.
(load "dired-loaddefs.el")

;;; Lazy-load feature settings
(dolist (filename (directory-files my-features-dir))
  (when (string-suffix-p ".el" filename)
    (let* ((feature-str (substring filename 3 (- (length filename) 3)))
           (feature (intern feature-str))
           (my-feature (intern (concat "my-" feature-str))))
      (message "Delaying loading %s" feature-str)
      (with-eval-after-load feature (require my-feature)))))

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
(with-eval-after-load 'comint (require 'my-shell))
(with-eval-after-load 'org (require 'my-org))
(with-eval-after-load 'auto-indent-mode (require 'my-auto-indent-mode))
(require 'my-text-mode)
(require 'my-prog-modes)

;;; Start emacs server
(server-start)

;;; Load local init.
(when (file-symlink-p my-local-dir)
  (load (expand-file-name "init.el" my-local-dir)))
