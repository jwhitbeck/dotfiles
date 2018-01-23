;;; -*- lexical-binding: t; -*-

;;; Clojure configuration

(require 'my-package)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(my-use-packages cider clojure-mode clj-refactor kibit-helper)

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'subword-mode)

;;; Use cider for org-babel Org-babel
(eval-when-compile
  (require 'org)
  (require 'ob-clojure))
(with-eval-after-load 'ob-clojure
  (custom-set-variables '(org-babel-clojure-backend 'cider)))

(eval-when-compile
  (require 'clojure-mode)
  (require 'cider))
(with-eval-after-load 'clojure-mode
  (custom-set-variables
   ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established.
   '(cider-repl-pop-to-buffer-on-connect nil)
   ;; Hide the cider process buffers
   '(nrepl-hide-special-buffers t)
   ;; Don't log all nrepl protocol messages-buffer
   '(nrepl-log-messages nil)
   ;; Auto-focus the error buffer when it's displayed after evaluating some clojure code. This makes it easy to
   ;; type "q" to dismiss the window, assuming you don't want this backtrace window hanging around.
   '(cider-auto-select-error-buffer t)
   ;; When using `cljr-clean-ns`, don't favor prefix notation in the ns form.
   '(cljr-favor-prefix-notation nil))

  ;; Disable C-c SPC binding in clojure mode as it overrides the ace-window-mode key binding
  (define-key clojure-mode-map (kbd "C-c SPC") nil))

(provide 'my-clojure)
