;;; -*- lexical-binding: t; -*-

;;;; Add custom lisp dirs to load-path
(require 'my-dirs (expand-file-name "me/my-dirs.el" user-emacs-directory))

;;;; Security settings for emacs.
;;;
;;; Even with these settings, emacs isn't very good at TLS security.
;;;
;;; References:
;;;  - https://www.gnu.org/software/emacs/manual/html_node/emacs-gnutls/Help-For-Users.html
;;;  - https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(require 'gnutls)

(setq
 ;; Ensure that certificates are verified.
 gnutls-verify-error t
 ;; Minimum number of prime bits accepted by GnuTLS for key exchange.
 gnutls-min-prime-bits 2048
 ;; Use cipher suites that enable perfect forward security.
 ;; https://gnutls.org/manual/html_node/Priority-Strings.html
 gnutls-algorithm-priority "PFS")

;;; I can test emacs TLS againt badssl urls using the following snippet.
;; (let ((no-error-urls '()))
;;   (dolist (url '("https://revoked.badssl.com/"  ; As of emacs 26.3, retrieved without error
;;                  "https://pinning-test.badssl.com/" ; As of emacs 26.3, retrieved without error
;;                  "https://expired.badssl.com/"
;;                  "https://wrong.host.badssl.com/"
;;                  "https://untrusted-root.badssl.com/"
;;                  "https://self-signed.badssl.com/"
;;                  "https://invalid-expected-sct.badssl.com/"
;;                  "https://dh1024.badssl.com/"))
;;     (when-let ((buf (ignore-errors (url-retrieve-synchronously url))))
;;       (push url no-error-urls)))
;;   no-error-urls)


;;;; Locale

;;; Use UTF-8 everywhere
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)


;;;; Performance

(setq
 ;; Reduce occurence of garbage collection
 gc-cons-threshold 20000000
 ;; Increase limit of lisp variable bindings
 max-specpdl-size 2500)


;;;; Customizations

;;; Keep the Emacs customization system out of init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Autoloads

;;; Regenerate using M-x update-directory-autoloads
(load (expand-file-name "my-loaddefs.el" my-dir))

;;; Read autoloads for advanced dired functions.
(load "dired-loaddefs.el")


;;;; Lazy-load feature settings
(dolist (filename (directory-files my-features-dir))
  (when (string-suffix-p ".el" filename)
    (let* ((feature-str (substring filename 3 (- (length filename) 3)))
           (feature (intern feature-str))
           (my-feature (intern (concat "my-" feature-str))))
      (message "Delaying loading %s" feature-str)
      (with-eval-after-load feature (require my-feature)))))


;;;; Packages

;;; This must come before configurations of installed packages. Emacs will
;;; insert this line if not present.
(package-initialize)

;;; Load theme. Must come first because it re-loads custom.el.
(unless (package-installed-p 'zenburn-theme)
  (my-packages-install 'zenburn-theme))
(load-theme 'zenburn t)

;;; Install packages on startup
(require 'my-packages)
(dolist (pkg my-packages)
  (my-packages-install pkg))

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
