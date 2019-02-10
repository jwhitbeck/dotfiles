;;; -*- lexical-binding: t; -*-

;;; package.el setup

(require 'my-security)
(require 'package)
(custom-set-variables
 '(package-archives '(("org" . "http://orgmode.org/elpa/")
                      ("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("melpa-stable" . "https://stable.melpa.org/packages/"))))

(defvar my-packages-refreshed? nil
  "t if we have refreshed the package info in this emacs session")

(defmacro my-use-packages (&rest packages)
  "Ensures the provided PACKAGES are available locally and
downloads them if necessary."
  `(dolist (pkg (quote (,@packages)))
     (when (not (package-installed-p pkg))
       (when (not my-packages-refreshed?)
         (package-refresh-contents)
         (setq my-packages-refreshed? t))
       (package-install pkg))))

;;; Ensure that we use the latest org-mode with contribs. This needs to be
;;; installed before all other packages.
(my-use-packages org-plus-contrib)

(provide 'my-package)
