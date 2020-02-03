;;; -*- lexical-binding: t; -*-

;;;; Settings for shell script mode
(require 'sh-script)

(add-hook 'sh-mode-hook 'my-whitespace-mode-default)
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)

(setq sh-basic-offset 2)

(provide 'my-sh-script)
