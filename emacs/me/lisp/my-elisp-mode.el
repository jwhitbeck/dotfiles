;;; -*- lexical-binding: t; -*-

;;;; Emacs lisp settings
(require 'elisp-mode)
(require 'flycheck)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode 'my-whitespace-mode-default)

;;; Disable emacs-lisp-checkdoc flycheck because it creates at lot of noise in
;;; my personal emacs config files
(defun my-disable-emacs-lisp-checkdoc ()
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

(add-hook 'emacs-lisp-mode-hook 'my-disable-emacs-lisp-checkdoc)

(provide 'my-elisp-mode)
