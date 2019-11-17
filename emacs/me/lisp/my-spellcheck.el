;;; -*- lexical-binding: t; -*-

;;; Spellcheckers

(require 'my-package)
(my-use-packages flycheck synosaurus)

(eval-when-compile
  (require 'flycheck))

;;; Enable Flycheck in all buffers
(global-flycheck-mode t)

;;; Enable files to opt-out of flycheck.
(put 'flycheck-mode 'safe-local-variable 'booleanp)

;;; XXX moved to my-text-mode
;;; Activate flyspell for all text modes
;;; (add-hook 'text-mode-hook 'flyspell-mode)

;;; Activate flyspell for comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; Disable emacs-lisp-checkdoc flycheck because it creates at lot of noise in
;;; my personal emacs config files
(custom-set-variables
 `(flycheck-checkers ,(delq 'emacs-lisp-checkdoc flycheck-checkers)))

;;; Prevent C-c $ from overriding org-mode's archive-subtree binding
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-c $") nil))

;;; Prevent C-c ! from overriding org-mode's org-time-stamp-inactive
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c !") nil))

(provide 'my-spellcheck)
