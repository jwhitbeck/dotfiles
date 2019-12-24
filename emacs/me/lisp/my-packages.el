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
    company-emacs-eclim
    company-restclient
    eclim
    edit-indirect
    ess
    ess-smart-underscore
    fill-column-indicator
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
    nov
    ob-restclient
    ox-gfm
    paredit
    powerline
    projectile
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
    yasnippet))

(setq package-pinned-packages
      '((auto-indent-mode . "melpa-stable")
        (cider . "melpa-stable")))

(defvar my-packages-refreshed? nil)

(defun my-packages-install (pkg)
  "Ensures the packages are installed. Refreshes package list if
  necessary."
  (unless (package-installed-p pkg)
    (unless my-packages-refreshed?
      (package-refresh-contents)
      (setq my-packages-refreshed? t))
    (package-install pkg)))

(provide 'my-packages)
