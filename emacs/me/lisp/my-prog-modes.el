;;; -*- lexical-binding: t; -*-

;;; Settings for programming language major modes that don't justify their own file

(require 'my-package)
(require 'my-editing)
(require 'my-spellcheck)
(my-use-packages ess ess-smart-underscore go-mode yaml-mode)

;;; Emacs Speaks Statistis
(eval-when-compile
  (require 'ess))
(with-eval-after-load 'ess
  (define-key ess-mode-map (kbd "_") 'ess-smarter-underscore))

;;; Golang
(add-hook 'go-mode-hook 'my-disable-tab-highlighting)
(add-hook 'go-mode-hook 'my-enable-indent-tabs)

;;; YAML
;; Add a few extra minor-modes since yaml-mode does not derive from either prog-mode or text-mode
(add-hook 'yaml-mode-hook 'fci-mode) ; show bar beyond the fill column
(add-hook 'yaml-mode-hook 'flyspell-mode) ; turn on automatic spell-checking

;;; CSS
(custom-set-variables
 '(css-indent-offset 2))

;;; SQL
;;; Add option to chose port in sql-postgres and use localhost as default server.
(custom-set-variables
 '(sql-postgres-login-params
   `((user :default ,(user-login-name))
     (database :default ,(user-login-name))
     (server :default "localhost")
     (port :default 5432))))

;;; MARKDOWN
(defun my-enable-orgtbl-mode ()
  (require 'org-table)
  (orgtbl-mode t))
(add-hook 'markdown-mode-hook 'my-enable-orgtbl-mode)
(add-to-list 'my-proselint-major-modes 'markdown-mode)

(provide 'my-prog-modes)
