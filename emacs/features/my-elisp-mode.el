;;; -*- lexical-binding: t; -*-

;;;; Emacs lisp settings
(require 'elisp-mode)
(require 'flycheck)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode 'my-whitespace-mode)

;;; Disable emacs-lisp-checkdoc flycheck because it creates at lot of noise in
;;; my personal emacs config files
(defun my-emacs-lisp--disable-checkdoc ()
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp--disable-checkdoc)

;;; Fast REPL pretty-printing
(define-key emacs-lisp-mode-map (kbd "C-c C-p") 'pp-eval-last-sexp)

(provide 'my-elisp-mode)
