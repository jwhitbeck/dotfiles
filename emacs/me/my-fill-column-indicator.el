;;; -*- lexical-binding: t; -*-

;;;; Fill-column indicator mode settings
(require 'fill-column-indicator)

;;; Workaround for fill-column indicator
;;; See https://github.com/company-mode/company-mode/issues/180
(require 'company)

(defvar-local my-company--fci-mode-on-p nil)

(defun my-company--turn-off-fci (&rest _)
  (when (boundp 'fci-mode)
    (setq my-company--fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun my-company--maybe-turn-on-fci (&rest _)
  (when my-company--fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'my-company--turn-off-fci)
(add-hook 'company-completion-finished-hook 'my-company--maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'my-company--maybe-turn-on-fci)

;;; unfill-paragraph, unfill-region
;;; http://ergoemacs.org/emacs/emacs_unfill-paragraph.html
;;;###autoload
(defun my-unfill-paragraph ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

;;;###autoload
(defun my-unfill-region (start end)
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(provide 'my-fill-column-indicator)
