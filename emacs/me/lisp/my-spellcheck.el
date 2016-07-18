;;; -*- lexical-binding: t; -*-

;;; Spellcheckers

(require 'my-package)
(my-use-packages flycheck)

(eval-when-compile
  (require 'flycheck))

;;; Enable Flycheck in all buffers
(global-flycheck-mode t)

;;; Activate flyspell for all text modes
(add-hook 'text-mode-hook 'flyspell-mode)

;;; Activate flyspell for comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; TODO(Sean): why do I need this?
(customize-set-variable 'flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))

;;; Proselint integration, see http://unconj.ca/blog/linting-prose-in-emacs.html
(defvar-local my-use-proselint nil
  "If t, enables proselint in buffer.")

(defcustom my-proselint-major-modes '(text-mode)
  "List of major modes that have proselint enabled."
  :type '(repeat symbol)
  :group 'my-spellcheck)

(when (executable-find "proselint")
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :predicate (lambda ()
                 (and (or (not ispell-current-dictionary)
                          ;; only use when typing in english
                          (string-prefix-p "en" ispell-current-dictionary))
                      (or my-use-proselint
                          (member major-mode my-proselint-major-modes)))))
  (add-to-list 'flycheck-checkers 'proselint))

;;; Prevent C-c $ from overriding org-mode's archive-subtree binding
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-c $") nil))

(provide 'my-spellcheck)
