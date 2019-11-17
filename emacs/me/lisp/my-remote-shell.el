;;; -*- lexical-binding: t; -*-

;;;; Helper functions for connecting to remote hosts

;;;###autoload
(defun my-remote-shell-at-point ()
  "Open a remote shell on the host-name under point."
  (interactive)
  (let ((hostname (thing-at-point 'filename)))
    (let ((buffer-name (format "*%s*" hostname))
          (default-directory (format "/%s:" hostname))
          (current-prefix-arg '-))      ; C-u
      (shell buffer-name))))

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
	(cl-pushnew host hosts)))
    hosts))

;;;###autoload
(defun my-remote-shell (hostname)
  "Open a tramp-enabled shell on HOSTNAME."
  (interactive
   (list (completing-read "user@host: "
                          (my-remote-shell-list-hosts))))
  (let ((buffer-name (format "*%s*" hostname))
        (default-directory (format "/scp:%s:" hostname))
        (current-prefix-arg '-))        ; C-u
    (shell buffer-name)))

(provide 'my-remote-shell)
