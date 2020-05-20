;;; -*- lexical-binding: t; -*-

;;; Clojure configuration
(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'clojure-mode-hook 'my-whitespace-mode-default)
(add-hook 'clojure-mode-hook 'auto-indent-mode)

(setq
 ;; Indent like a macro body unless first arg is on the same line.
 clojure-indent-style :align-arguments)

;; Disable C-c SPC binding in Clojure mode as it overrides the
;; ace-window-mode key binding
(define-key clojure-mode-map (kbd "C-c SPC") nil)

(provide 'my-clojure-mode)
