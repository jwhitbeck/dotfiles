;;; -*- lexical-binding: t; -*-

;;; Clojure configuration
(require 'cider)
(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'clojure-mode-hook 'my-whitespace-mode-default)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'subword-mode)

(setq
 ;; Prevent the auto-display of the REPL buffer in a separate window
 ;; after connection is established.
 cider-repl-pop-to-buffer-on-connect nil
 ;; Hide the cider process buffers
 nrepl-hide-special-buffers t
 ;; Indent like a macro body unless first arg is on the same line.
 clojure-indent-style :align-arguments)

;; Disable C-c SPC binding in Clojure mode as it overrides the
;; ace-window-mode key binding
(define-key clojure-mode-map (kbd "C-c SPC") nil)

(provide 'my-clojure-mode)
