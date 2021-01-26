;;; -*- lexical-binding: t; -*-

;;;; Window management functions

;;;###autoload
(defun my-windows-rotate-split-horizontal ()
  "Rotate windows from a 2-window vertical split to a 2-window horizontal split."
  (interactive)
  (when (and (= (count-windows) 2)
             ;; true if current split is horizontal
             (caar (window-tree)))
    (let* ((cur-win (selected-window))
           (other-win (next-window))
           (cur-buffer (window-buffer))
           (other-buffer (window-buffer other-win))
           (cur-edges (window-edges cur-win))
           (other-edges (window-edges other-win)))
      (delete-other-windows)
      (split-window-horizontally)
      ;; True if current buffer was initially below the other buffer.
      (when (> (cadr cur-edges) (cadr other-edges))
        (other-window 1))
      (set-window-buffer (selected-window) cur-buffer)
      (set-window-buffer (next-window) other-buffer))))

;;;###autoload
(defun my-windows-rotate-split-vertical ()
  "Rotate windows from a 2-window horizontal split to a 2-window vertical split."
  (interactive)
  (when (and (= (count-windows) 2)
             ;; true if current split is horizontal
             (not (caar (window-tree))))
    (let* ((cur-win (selected-window))
           (other-win (next-window))
           (cur-buffer (window-buffer))
           (other-buffer (window-buffer other-win))
           (cur-edges (window-edges cur-win))
           (other-edges (window-edges other-win)))
      (delete-other-windows)
      (split-window-vertically)
      ;; True if current buffer was initially to the right of the other buffer.
      (when (> (car cur-edges) (car other-edges))
        (other-window 1))
      (set-window-buffer (selected-window) cur-buffer)
      (set-window-buffer (next-window) other-buffer))))

;;;###autoload
(defun my-windows-switch ()
  "Switch the buffers displayed in a 2-window split."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((cur-buffer (window-buffer))
           (other-buffer (window-buffer (next-window))))
      (set-window-buffer (next-window) cur-buffer)
      (set-window-buffer (selected-window) other-buffer)
      (select-window (next-window)))))

;;;###autoload
(defun my-windows-split-below ()
  "Split the window vertically and move focus to the new window
below the selected one."
  (interactive)
  (split-window-below)
  (other-window 1))

;;;###autoload
(defun my-windows-split-right ()
  "Split the window horizontal and move focus to the new window
to the right of the selected one."
  (interactive)
  (split-window-right)
  (other-window 1))

;;; Lock buffers to windows
(require 'face-remap)

(defvar-local my-windows--face-remap-cookie nil)

;;;###autoload
(defun my-windows-toggle-dedicated ()
  "Toggle whether or not the window is dedicated to its buffer."
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p)))
  (if (window-dedicated-p)
      (setq my-windows--face-remap-cookie
            (face-remap-add-relative 'mode-line-buffer-id 'warning))
    (face-remap-remove-relative my-windows--face-remap-cookie)))

(provide 'my-windows)
