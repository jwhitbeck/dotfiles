;;; -*- lexical-binding: t; -*-

;;; Shell mode customization

(require 'comint)
(require 'term)

(setq
 comint-scroll-to-bottom-on-input t   ; Always insert at the bottom.
 comint-move-point-for-output nil     ; Always add output at the bottom.
 comint-scroll-show-maximum-output t  ; Scroll to show max possible output.
 comint-completion-autolist t         ; Show completion list when ambiguous.
 comint-input-ignoredups t            ; No duplicates in command history.
 comint-completion-addsuffix t        ; Insert space/slash after file completion.
 comint-get-old-input (lambda () "")  ; What gets sent to prompt when pressing enter in the buffer.
 comint-buffer-maximum-size 20000     ; Max length of buffer in lines.
 comint-input-ring-size 5000          ; Max shell history size.
 explicit-shell-file-name "/bin/bash") ; Always use bash on remote hosts.

;; Do not use `less` as the default pager.
(setenv "PAGER" "cat")

;; Truncate buffers continuously.
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; Disable C-c SPC binding in comint mode as it overrides the ace-window-mode
;; key binding and provides functionality that is redundant with C-j.
(define-key comint-mode-map  (kbd "C-c SPC") nil)

;;; Shell buffer management

;;;###autoload
(defun my-shell-switchb ()
  (interactive)
  (let ((bufs '()))
    (dolist (buf (buffer-list))
      (when (and (eq 'shell-mode (buffer-local-value 'major-mode buf))
                 (comint-check-proc buf))
        (push buf bufs)))
    (if (null bufs)
        (message "No active shell buffers")
      (pop-to-buffer-same-window
       (completing-read "Shell buffer: "
                        (mapcar 'buffer-name bufs)
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
