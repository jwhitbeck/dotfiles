;;; -*- lexical-binding: t; -*-

;;;; Fill-column indicator mode settings
(require 'fill-column-indicator)

;;; Workaround for fill-column indicator
;;; See https://github.com/company-mode/company-mode/issues/180
(require 'company)

(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest _)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest _)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

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
