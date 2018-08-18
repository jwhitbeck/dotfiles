;;; -*- lexical-binding: t; -*-

;;; Settings for programming language major modes that don't justify their own file

(require 'my-package)
(require 'my-editing)
(require 'my-spellcheck)
(my-use-packages
 auto-indent-mode
 ess
 ess-smart-underscore
 geiser
 go-eldoc
 go-mode
 ini-mode
 markdown-mode
 protobuf-mode
 toml-mode
 yaml-mode)

;;; Emacs Speaks Statistis
(eval-when-compile
  (require 'ess))
(with-eval-after-load 'ess
  (define-key ess-mode-map (kbd "_") 'ess-smarter-underscore))

;;; Makefile
(my-disable-tab-highlighting 'makefile-gmake-mode)

;;; Scheme
(custom-set-variables
 '(geiser-active-implementations '(chicken)))
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)
(add-hook 'scheme-mode-hook 'subword-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'subword-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)

;;; Redefine the chicken scheme flycheck checker to handle more cases.
;;; TODO: Remove once this is fixed upstream.
(flycheck-define-checker scheme-chicken
  "A CHICKEN Scheme syntax checker using the CHICKEN compiler `csc'.

See URL `http://call-cc.org/'."
  :command ("csc" "-analyze-only" "-local"
            (eval flycheck-scheme-chicken-args)
            source)
  :error-patterns
  ((info line-start
         "Note: " (zero-or-more not-newline) ":\n"
         (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
         line-end)
   ;; NOTE: missing in flycheck.el
   (warning line-start
            "Warning: " (zero-or-more not-newline) ",\n"
            (one-or-more (any space)) (zero-or-more not-newline) ":\n"
            (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
            line-end)
   (warning line-start
            "Warning: " (zero-or-more not-newline) ":\n"
            (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
            line-end)
   (error line-start "Error: (line " line ") " (message) line-end)
   (error line-start "Syntax error: (" (file-name) ":" line ")"
          (zero-or-more not-newline) " - "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (zero-or-more space)
                                 (zero-or-more not-newline))
                   (one-or-more space) "<--")
          line-end)
   (error line-start
          "Error: " (zero-or-more not-newline) ":\n"
          (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
          line-end)
   ;; NOTE: Missing in flycheck.el
   (error line-start
          "Error: " (file-name) ":" line ": " (message)
          line-end))
  :predicate
  (lambda ()
    ;; In `scheme-mode' we must check the current Scheme implementation
    ;; being used
    (and (boundp 'geiser-impl--implementation)
         (eq geiser-impl--implementation 'chicken)))
  :verify
  (lambda (_checker)
    (let ((geiser-impl (bound-and-true-p geiser-impl--implementation)))
      (list
       (flycheck-verification-result-new
        :label "Geiser Implementation"
        :message (cond
                  ((eq geiser-impl 'chicken) "Chicken Scheme")
                  (geiser-impl (format "Other: %s" geiser-impl))
                  (t "Geiser not active"))
        :face (cond
               ((eq geiser-impl 'chicken) 'success)
               (t '(bold error)))))))
  :modes scheme-mode)

;;; Golang
(my-disable-tab-highlighting 'go-mode)
(add-hook 'go-mode-hook 'my-enable-indent-tabs)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(custom-set-variables
 '(gofmt-command "goimports"))

(eval-when-compile
  (require 'go-mode))
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "M-.") 'godef-jump))

(defun my-enable-gofmt-before-save ()
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook 'my-enable-gofmt-before-save)

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
(my-enable-proselint-for-mode 'markdown-mode)

(provide 'my-prog-modes)
