;;; -*- lexical-binding: t; -*-

;;; Some useful commands
(eval-when-compile
  (require 'dired))

;;; Print current UTC time in echo area
(defun my-current-utc-time ()
  "Returns an ISO 8061 UTC timestamp"
  (interactive)
  (set-time-zone-rule t)
  (let ((timestamp (prog1 (format-time-string "%Y-%m-%d-T%TZ")
                     (set-time-zone-rule nil))))
    (if (called-interactively-p 'interactive)
        (message "%s" timestamp)
      timestamp)))

(global-set-key (kbd "C-c u") 'my-current-utc-time)

;;; Run commands in the background and output to a specific buffer
(defvar-local my-refreshable-shell-command--command nil)

(defun my-refreshable-shell-command--refresh ()
  (interactive)
  (when my-refreshable-shell-command--command
    (let ((cmd my-refreshable-shell-command--command)
          (inhibit-read-only t))
      (erase-buffer)
      (let ((handler (find-file-name-handler
                      (directory-file-name default-directory)
                      'shell-command)))
        (if handler (funcall handler 'shell-command cmd t nil)
          (call-process shell-file-name nil t nil shell-command-switch cmd))))))

(defun my-refreshable-shell-command (command buffer-or-name)
  "Run command synchronously and send output to the provided
buffer. The buffer will be marked read-only. Press 'g' to refresh
the output of the command. Press 'q' to dismiss the buffer."
  (interactive
   (let ((cmd (read-shell-command "Shell command: " nil nil))
         (buffer-name (read-string "Output to buffer: ")))
     (list cmd buffer-name)))
  (let ((buf (get-buffer-create buffer-or-name))
        (directory default-directory))
    (with-current-buffer buf
      (read-only-mode)
      (setq default-directory directory)
      (local-set-key (kbd "g") 'my-refreshable-shell-command--refresh)
      (local-set-key (kbd "q") 'kill-this-buffer)
      (setq my-refreshable-shell-command--command command)
      (my-refreshable-shell-command--refresh))
    (set-window-buffer nil buf)))

;;; Run commands in a detached process
(defun my-detached-shell-command (command)
  "Like shell-command but runs the shell command in a process
detached from emacs."
  (interactive
   (list (let ((args (let ((filename (cond
                                       (buffer-file-name)
                                       ((eq major-mode 'dired-mode)
                                        (dired-get-filename nil t)))))
                       (and filename (file-relative-name filename)))))
           (read-shell-command "Detached shell command: " nil nil args))))
  (let ((handler (find-file-name-handler (directory-file-name default-directory)
                                         'shell-command)))
    (if handler (funcall handler 'my-detached-shell-command command)
      (call-process shell-file-name nil 0 nil shell-command-switch command))))

(global-set-key (kbd "M-§") 'my-detached-shell-command)

;;; Quickly open shells on remote hosts.
(defun my-remote-shell-at-point ()
  "Open a remote shell on the host-name under point."
  (interactive)
  (let ((hostname (thing-at-point 'filename)))
    (let ((buffer-name (format "*%s*" hostname))
          (default-directory (format "/%s:" hostname))
          (current-prefix-arg '-))      ; C-u
      (shell buffer-name))))

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

(defcustom my-list-remote-hosts-function 'my-tramp-connection-history
  "A function that returns the list of hosts to consider for `my-remote-shell'."
  :type 'function
  :group 'my-commands)

(defun my-remote-shell (hostname)
  "Open a tramp-enabled shell on HOSTNAME."
  (interactive
   (list (ido-completing-read "user@host: "
                              (funcall my-list-remote-hosts-function))))
  (let ((buffer-name (format "*%s*" hostname))
        (default-directory (format "/scp:%s:" hostname))
        (current-prefix-arg '-))        ; C-u
    (shell buffer-name)))

(global-set-key (kbd "C-c s") 'my-remote-shell)

;;; X11 Notifications
;;; Assumes that emacs is running on ubuntu
;;; Adapted from http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
(defun my-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the
title of the message, MSG is the context. Optionally, you can
provide an ICON and SOUND."
  (interactive
   (list (read-from-minibuffer "Title: ")
         (read-from-minibuffer "Message: ")))
  (when sound
    (call-process "pacmd" nil 0 nil "play-file" sound "0"))
  (if (eq window-system 'x)
      (let ((args (if icon (list "-i" icon title msg) (list title msg))))
        (apply 'call-process "notify-send" nil 0 nil args))
    ;; text only version
    (message (format "%s: %s" title msg))))

(provide 'my-commands)
