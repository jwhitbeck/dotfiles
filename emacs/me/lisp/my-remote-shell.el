;;; -*- lexical-binding: t; -*-

;;;; Helper functions for connecting to remote hosts

;;;###autoload
(defun my-tramp-connection-history ()
  "Return the list of all user@hostname pairs present in the
tramp connection history."
  (let ((tramp-conn-hist (with-temp-buffer
                           (insert-file-contents-literally "~/.emacs.d/tramp")
                           (read (current-buffer))))
        (conns '()))
    (dolist (conn-spec tramp-conn-hist)
      (when conn-spec
        (let* ((conn-head (car conn-spec))
               (user (nth 2 conn-head))
               (host (nth 4 conn-head)))
          (when (and host (not (equal "root" user)))
            (if user
                (push (format "%s@%s" user host) conns)
              (push host conns))))))
    conns))

;;;###autoload
(defvar my-remote-shell-list-hosts-functions
  '(my-tramp-connection-history)
  "A list of functions that return lists of hosts to consider for
  `my-remote-shell'.")

(defun my-remote-shell-list-hosts ()
  (let (hosts)
    (dolist (fun my-remote-shell-list-hosts-functions)
      (dolist (host (funcall fun))
	(unless (member host hosts)
	  (push host hosts))))
    hosts))

;;;###autoload
(defun my-remote-shell (hostname)
  "Open a tramp-enabled shell on HOSTNAME."
  (interactive
   (list (completing-read "user@host: "
                          (my-remote-shell-list-hosts)
                          nil nil nil nil
                          (thing-at-point 'filename))))
  (let* ((buffer-name (format "*%s*" hostname))
         (buf (get-buffer buffer-name)))
    (unless buf
      (setq buf (get-buffer-create buffer-name))
      (with-current-buffer buf
        (setq-local default-directory (format "/scp:%s:" hostname))))
    (pop-to-buffer-same-window buf)
    (shell buf)))

(provide 'my-remote-shell)
