;;; -*- lexical-binding: t; -*-

;;;; Flyscheck configuration
(require 'flycheck)

;;; Enable files to opt-out of flycheck.
(put 'flycheck-mode 'safe-local-variable 'booleanp)

;;; Ensure that flycheck uses the customized emacs lisp load-path, not the
;;; default one.
;;; https://stackoverflow.com/questions/20498554/how-do-i-make-flycheck-find-required-file-in-emacs-lisp
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(provide 'my-flycheck)
