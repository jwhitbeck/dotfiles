;;; -*- lexical-binding: t; -*-

;;;; Settings for shell script mode

(add-hook 'sh-mode-hook 'my-whitespace-mode-default)

(setq sh-basic-offset 2)

(provide 'my-sh-script)
