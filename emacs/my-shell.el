;;; -*- lexical-binding: t; -*-

;;; Shell buffer management
(require 'comint)

;;;###autoload
(defun my-shell-switchb ()
  (interactive)
  (let ((bufs '()))
    (dolist (buf (buffer-list))
      (when (and (eq 'shell-mode (buffer-local-value 'major-mode buf))
                 (comint-check-proc buf)
                 ;; Only list interactive shell buffers
                 (member "-i" (with-current-buffer buf
                                (process-command (get-buffer-process buf)))))
        (push buf bufs)))
    (if (null bufs)
        (message "No active shell buffers")
      (pop-to-buffer-same-window
       (completing-read "Shell buffer: "
                        (mapcar #'buffer-name bufs)
                        nil t)))))

;;;###autoload
(defun my-shell (&optional arg)
  "Like `shell' but always opens the *shell* buffer in the
current window. With a C-u prefix, opens a new shell. With C-u
C-u prefix prompts for the new buffer's name. If the buffer
doesn't exist and no prefix arg is given, create it in the home
directory."
  (interactive "P")
  (let* ((buf-name (cond ((null arg) "*shell*")
                        ((equal arg '(4)) (generate-new-buffer-name "*shell*"))
                        ((equal arg '(16)) (read-buffer "Name: "))
                        (t (error "Invalid arg %s" arg))))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (when (null arg)
        (with-current-buffer buf
          (setq-local default-directory "~"))))
    (pop-to-buffer-same-window buf)
    (shell buf)))

(provide 'my-shell)
