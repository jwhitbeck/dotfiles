;;; -*- lexical-binding: t; -*-

;;;; Shell settings

(require 'shell)
(require 'company)

(defun my-shell--disable-remote-idle-complete ()
  (when (string-prefix-p "/scp:" comint-file-name-prefix)
    (setq-local company-idle-delay nil)))

(add-hook 'shell-mode-hook 'my-shell--disable-remote-idle-complete)

(provide 'my-shell)
