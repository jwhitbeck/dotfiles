;;; -*- lexical-binding: t; -*-

;;; Clojure configuration
(require 'cider)

(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(setq
 ;; Prevent the auto-display of the REPL buffer in a separate window
 ;; after connection is established.
 cider-repl-pop-to-buffer-on-connect nil
 ;; Hide the cider process buffers
 nrepl-hide-special-buffers t)

(provide 'my-cider)
