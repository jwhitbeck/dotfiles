;;; -*- lexical-binding: t; -*-

;;;; Shell settings

(require 'shell)
(require 'company)
(require 'bash-completion)

(defun my-shell--disable-remote-idle-complete ()
  (when (string-prefix-p "/scp:" comint-file-name-prefix)
    (setq-local company-idle-delay nil)))

(add-hook 'shell-mode-hook 'my-shell--disable-remote-idle-complete)

(bash-completion-setup)

;;; Don't let shell-mode rebind TAB, which is globally bound to
;;; company-indent-or-complete-common
(define-key shell-mode-map (kbd "TAB") nil)

(provide 'my-shell)
