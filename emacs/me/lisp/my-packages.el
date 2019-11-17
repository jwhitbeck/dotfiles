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
    ace-jump-mode
    ace-window
    async
    auto-indent-mode
    browse-at-remote
    cider
    clojure-mode
    company
    company-restclient
    ess
    ess-smart-underscore
    fill-column-indicator
    fiplr
    flx-ido
    flycheck
    geiser
    go-eldoc
    go-mode
    hide-lines
    htmlize
    ido
    ido-completing-read+
    ido-vertical-mode
    ini-mode
    magit
    markdown-mode
    ob-restclient
    paredit
    peep-dired
    protobuf-mode
    rainbow-delimiters
    restclient
    scratch
    smex
    string-inflection
    synosaurus
    toml-mode
    undo-tree
    visual-fill-column
    yaml-mode
    yasnippet
    zenburn-theme))

(setq package-pinned-packages
      '((auto-indent-mode . "melpa-stable")
        (cider . "melpa-stable")))

;;; Install packages on startup
(let ((refreshed? nil))
  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (unless refreshed?
        (package-refresh-contents)
        (setq refreshed? t))
      (package-install pkg))))

(provide 'my-packages)
