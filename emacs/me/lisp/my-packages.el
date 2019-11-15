;;; -*- lexical-binding: t; -*-

;;;; package.el setup

(require 'package)

(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar my-packages
  '(;; Ensure that we use the latest org-mode with contribs. This needs to be
    ;; installed before all other packages.
    org-plus-contrib
    auto-indent-mode
    cider
    clojure-mode))

(setq package-pinned-packages
      '((cider . "melpa-stable")))

;;; Install packages on startup
(let ((refreshed? nil))
  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (unless refreshed?
        (package-refresh-contents)
        (setq refreshed? t))
      (package-install pkg))))

(provide 'my-packages)
