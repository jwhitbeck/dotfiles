;;; -*- lexical-binding: t; -*-

;;;; Cycle between word-wrapping settings
(require 'visual-fill-column)

;;;###autoload
(defun my-word-wrap (&optional arg)
  (interactive "p")
  (cl-case arg
    ;; long lines
    (1 (progn (toggle-truncate-lines 1)
	      (toggle-word-wrap 0)
	      (visual-fill-column-mode 0)))
    ;; wrap words at window boundary
    (4 (progn (toggle-truncate-lines 0)
	      (toggle-word-wrap 1)
	      (visual-fill-column-mode 0)))
    ;; wrap words at fill-column
    (16 (progn (toggle-truncate-lines 0)
	       (toggle-word-wrap 1)
	       (visual-fill-column-mode 1)))))

(provide 'my-word-wrap)
