;;; -*- lexical-binding: t; -*-

;;; package.el setup

(require 'my-security)
(require 'package)
(custom-set-variables '(package-archives '(("org" . "http://orgmode.org/elpa/")
                                           ("gnu" . "https://elpa.gnu.org/packages/")
                                           ("melpa" . "https://melpa.org/packages/")
                                           ("melpa-stable" . "https://stable.melpa.org/packages/"))))

;;; Refresh the package info if it has never been run before.
(when (not package-archive-contents)
  (package-refresh-contents))

(defmacro my-use-packages (&rest packages)
  "Ensures the provided PACKAGES are available locally and downloads them if necessary."
  `(dolist (pkg (quote (,@packages)))
     (when (not (package-installed-p pkg))
       (package-install pkg))))

;;; Ensure that we use the latest org-mode with contribs. This needs to be installed before all other
;;; packages.
(my-use-packages org-plus-contrib)

(provide 'my-package)
