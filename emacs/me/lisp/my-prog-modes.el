;;; -*- lexical-binding: t; -*-

;;; Settings for programming language major modes that don't justify their own
;;; file

(require 'my-package)
(require 'my-editing)
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
 scratch
 toml-mode
 yaml-mode)

;;; Emacs Speaks Statistis
(eval-when-compile
  (require 'ess))
(with-eval-after-load 'ess-mode
  (define-key ess-mode-map (kbd "_") 'ess-smarter-underscore))

;;; Makefile
;; XXX re-enable
;; (my-disable-tab-highlighting 'makefile-gmake-mode)

;;; Scheme
(custom-set-variables
 '(geiser-active-implementations '(chicken)))
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)
(add-hook 'scheme-mode-hook 'subword-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'subword-mode)
(add-hook 'scheme-mode-hook 'auto-indent-mode)

;;; Golang
;; XXX
;; (my-disable-tab-highlighting 'go-mode)
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
;; Add a few extra minor-modes since yaml-mode does not derive from either
;; prog-mode or text-mode
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

;;;; Copied from other files

;;; Automatically show opened/closed parentheses
(show-paren-mode)

;;; Turn on syntax highlighting for all modes that support it.
(global-font-lock-mode t)

;;; Parentheses
(custom-set-variables
 '(show-paren-delay 0)                              ; immediately show matching parentheses
 '(show-paren-style 'expression))                   ; highlight full expression contained between parentheses
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; activate rainbow delimeters in prog mode

;;; Fill column indicator
(custom-set-variables
 '(fill-column 80))                    ; line wrap at 80 characters
(add-hook 'prog-mode-hook 'fci-mode)   ; show a bar beyond the fill-column

;;; Ensure that flycheck uses the customized load-path
;; https://stackoverflow.com/questions/20498554/how-do-i-make-flycheck-find-required-file-in-emacs-lisp
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(with-eval-after-load 'fill-column-indicator
  (require 'my-fill-column-indicator))

;;; XXX move to prog-modes
;;; Remove the auto-indent hook that disables electric-indent
(require 'auto-indent-mode)
(remove-hook 'after-change-major-mode-hook 'auto-indent-disable-electric)

;;; Highlight and auto-correct whitespace problems
(add-hook 'prog-mode-hook 'my-whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Activate undo tree for text and program editing
(add-hook 'prog-mode-hook 'undo-tree-mode)


(defcustom my-whitespace-styles nil
  "List of (major-mode . whitespace-style) pairs. Used to define
custom whitespace-mode styles by major mode."
  :type '(alist :key-type symbol :value-type (list symbol))
  :group 'my-editing)

(defcustom my-default-whitespace-style '(face empty trailing tabs tab-mark)
  "The default whitespace style to use."
  :type '(list symbol)
  :group 'my-editing)

;;; For the ->> macro
(require 'dash)

(defun my-disable-tab-highlighting (mm)
  "Disable tab highlighting for major mode."
  (add-to-list 'my-whitespace-styles
               (cons mm (->> my-default-whitespace-style
                             copy-sequence
                             (delq 'tabs)
                             (delq 'tab-mark)))))

(defun my-whitespace-mode ()
  "Configure whitespace-mode differently depending on the major mode."
  (setq-local whitespace-style (or (cdr (assoc major-mode my-whitespace-styles))
                                   my-default-whitespace-style))
  (whitespace-mode))



;;; No tabs by default. Modes that really need tabs should enable
;;; indent-tabs-mode explicitly.  Makefile-mode already does that, for example.
(custom-set-variables
 '(indent-tabs-mode nil))

(defun my-enable-indent-tabs ()
  "Enable using tabs for indentation."
  (setq indent-tabs-mode t))

;;; XXX consider removing
;;; If indent-tabs-mode is off, untabify before saving.
(defun my-untabify-buffer ()
  "Replace all tabs with spaces in buffer."
  (unless indent-tabs-mode
    (untabify (point-min) (point-max)))
  nil)

(add-hook 'write-file-hooks 'my-untabify-buffer)

;;; Turn on auto revert mode
; automatically revert a buffer when a file is changed on disk
(add-hook 'prog-mode-hook 'auto-revert-mode)

;;; Enable Flycheck in all buffers
(global-flycheck-mode t)

;;; Enable files to opt-out of flycheck.
(put 'flycheck-mode 'safe-local-variable 'booleanp)

;;; XXX moved to my-text-mode
;;; Activate flyspell for all text modes
;;; (add-hook 'text-mode-hook 'flyspell-mode)

;;; Activate flyspell for comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)



(provide 'my-prog-modes)
