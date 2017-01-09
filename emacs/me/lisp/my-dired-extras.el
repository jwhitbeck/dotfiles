;;; -*- lexical-binding: t; -*-

;;; Extra dired configuration loaded on-demand after dired itself is loaded

(require 'my-commands)
(require 'dired-x)
(require 'dired-aux)
(require 'dired-async)

(defun my-enable-dired-omit-mode ()
  "Enable dired-omit-mode"
  (dired-omit-mode t))

;;; Hide files that start with . and have a three least chars.
(custom-set-variables
 '(dired-omit-files "^\\...+$"))

(add-hook 'dired-mode-hook 'my-enable-dired-omit-mode)

;;; Use async file operations
(defun my-enable-dired-async-mode ()
  "Enable dired-async-mode"
  (dired-async-mode t))

(add-hook 'dired-mode-hook 'my-enable-dired-async-mode)

;;; Tail files in custom buffer
(defun my-tail-filename (filename &optional output-buffer)
  "Run tail -f on FILENAME and send output to OUTPUT-BUFFER. If
not specified, OUTPUT-BUFFER defaults to a newly created buffer called '*Tail FILENAME*'."
  (interactive
   (let* ((filename (dired-get-filename t))
          (output-buffer (read-string (format "Output %s to buffer: " filename)
                                      nil
                                      nil
                                      (format "*Tail %s*" filename))))
     (list filename output-buffer)))
  (let ((command (format "tail -n1000 -f %s &" (shell-quote-argument filename)))
        (handler (find-file-name-handler (directory-file-name default-directory) 'shell-command)))
    (if handler (apply handler 'shell-command (list command output-buffer))
      (shell-command command output-buffer)))
  (with-current-buffer output-buffer
    (setq truncate-lines t)))
(define-key dired-mode-map (kbd "C-c t") 'my-tail-filename)

;;; Better default programs for opening files
(custom-set-variables '(dired-guess-shell-alist-user
                        '(("\\.e?ps\\'" "evince")
                          ("\\.mpe?g\\'\\|\\.avi\\'" "vlc")
                          ("\\.ogg\\'" "vlc")
                          ("\\.mp3\\'" "vlc")
                          ("\\.m4a\\'" "vlc")
                          ("\\.wav\\'" "vlc")
                          ("\\.p[bgpn]m\\'" "eog")
                          ("\\.gif\\'" "eog")
                          ("\\.tif\\'" "eog")
                          ("\\.png\\'" "eog")
                          ("\\.jpe?g\\'" "eog")
                          ("\\.pdf\\'" "evince")
                          ("\\.doc\\'" "libreoffice")
                          ("\\.odt\\'" "libreoffice")
                          ("\\.mobi\\'" "ebook-viewer")
                          ("\\.epub\\'" "ebook-viewer")
                          ("\\.html\\'" "firefox")
                          ("\\.maff\\'" "firefox"))))

;;; Detached command-on-file execution
(defun my-dired-run-detached-shell-command (command)
  (let ((handler
         (find-file-name-handler (directory-file-name default-directory)
                                 'shell-command)))
    (if handler (apply handler 'my-detached-shell-command (list command))
      (my-detached-shell-command command)))
  ;; Return nil for sake of nconc in dired-bunch-files.
  nil)

(defun my-dired-do-detached-shell-command (command &optional arg file-list)
  "Like dired-do-shell-command, but detaches the processes."
  ;;Functions dired-run-shell-command and dired-shell-stuff-it do the
  ;;actual work and can be redefined for customization.
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "§ on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (let* ((on-each (not (string-match-p dired-star-subst-regexp command)))
         (no-subst (not (string-match-p dired-quark-subst-regexp command)))
         (star (string-match-p "\\*" command))
         (qmark (string-match-p "\\?" command)))
    ;; Get confirmation for wildcards that may have been meant
    ;; to control substitution of a file name or the file name list.
    (if (cond ((not (or on-each no-subst))
               (error "You can not combine `*' and `?' substitution marks"))
              ((and star on-each)
               (y-or-n-p "Confirm--do you mean to use `*' as a wildcard? "))
              ((and qmark no-subst)
               (y-or-n-p "Confirm--do you mean to use `?' as a wildcard? "))
              (t))
        (if on-each
            (dired-bunch-files
             (- 10000 (length command))
             (function (lambda (&rest files)
                         (dolist (file files)
                           (my-dired-run-detached-shell-command
                            (concat command dired-mark-separator (shell-quote-argument file))))))
             nil
             file-list)
          ;; execute the shell command
          (my-dired-run-detached-shell-command
           (dired-shell-stuff-it command file-list nil arg))))))

(defun my-dired-do-xdg-open (&optional arg file-list)
  "Wrapper around my-dired-do-detached-shell-command that always uses the xdg-open command."
  (interactive
   (list current-prefix-arg
         (dired-get-marked-files t current-prefix-arg)))
  (my-dired-do-detached-shell-command "xdg-open" current-prefix-arg file-list))

(define-key dired-mode-map (kbd "§") 'my-dired-do-detached-shell-command)
(define-key dired-mode-map (kbd "²") 'my-dired-do-xdg-open)

;;; Mu4e integration
(autoload 'my-ensure-mu4e-is-running "my-mu4e")

(defun my-gnus-dired-attach ()
  "Like gnus-dired-attach but starts mu4e in background if it isn't already running."
  (interactive)
  (my-ensure-mu4e-is-running)
  (turn-on-gnus-dired-mode)
  (call-interactively 'gnus-dired-attach))

(define-key dired-mode-map (kbd "a") 'my-gnus-dired-attach)

(defun my-gnus-dired-mail-buffers ()
  "Return a list of active mail composition buffers. Drop-in replacement for gnus-dired-mail-buffers."
  (require 'message)
  (message-buffers))

(advice-add 'gnus-dired-mail-buffers :override 'my-gnus-dired-mail-buffers)

;;; Miscellaneous settings
(custom-set-variables
 '(dired-auto-revert-buffer t)          ; Auto-revert
 '(dired-listing-switches "-alh"))      ; Human-readable file sizes

(provide 'my-dired-extras)
