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

(defun my-shell--gen-buffer-name ()
  (let ((stem (if (not (eq 'shell-mode major-mode))
                  "*shell*"
                (let ((bufname (buffer-name)))
                  (with-temp-buffer
                    (insert bufname)
                    (if (re-search-backward "<[[:digit:]]+>" nil t)
                        (buffer-substring (point-min) (point))
                      bufname))))))
    (generate-new-buffer-name stem)))

;;;###autoload
(defun my-shell (&optional arg)
  "Like `shell' but always opens the *shell* buffer in the
current window.

With a C-u prefix, starts a shell in a new buffer. If the current
buffer is a shell buffer, the new one will be named after
it (e.g., *foo*<2> if the current buffer is called
*foo*). Otherwise, use *shell*<N>.

With C-u C-u prefix, always prompts for the new buffer's name.

If the buffer doesn't exist and no prefix arg is given, set the
directory to the the home directory."
  (interactive "P")
  (let* ((buf-name (cond ((null arg) "*shell*")
                         ((equal arg '(4)) (my-shell--gen-buffer-name))
                         ((equal arg '(16)) (read-string "Buffer name: " (my-shell--gen-buffer-name)))
                        (t (error "Invalid arg %s" arg))))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (when (null arg)
        (with-current-buffer buf
          (setq-local default-directory "~"))))
    (pop-to-buffer-same-window buf)
    (shell buf)))

(provide 'my-shell-buffers)
