;;; -*- lexical-binding: t; -*-

;;;; Dired settings

;;; Extra dired configuration loaded on-demand after dired itself is loaded

(require 'dired-x)
(require 'dired-aux)
(require 'dired-async)
(require 'my-external-programs)

;;; Hide files that start with . and have a three least chars.
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook 'dired-omit-mode)

;;; Tail files in custom buffer
(defun my-tail-filename (filename &optional output-buffer)
  "Run tail -f on FILENAME and send output to OUTPUT-BUFFER. If
not specified, OUTPUT-BUFFER defaults to a newly created buffer
called '*Tail FILENAME*'."
  (interactive
   (let* ((filename (dired-get-filename t))
          (output-buffer (read-string (format "Output %s to buffer: " filename)
                                      nil
                                      nil
                                      (format "*Tail %s*" filename))))
     (list filename output-buffer)))
  (let ((command (format "tail -n1000 -f %s &" (shell-quote-argument filename)))
        (handler (find-file-name-handler (directory-file-name default-directory)
                                         'shell-command)))
    (if handler (apply handler 'shell-command (list command output-buffer))
      (shell-command command output-buffer)))
  (with-current-buffer output-buffer
    (setq truncate-lines t)))

(define-key dired-mode-map (kbd "C-c t") 'my-tail-filename)

(defun my-detached-shell-command (command)
  "Like shell-command but runs the shell command in a process
detached from emacs."
  (interactive
   (list (let* ((filename (cond
                           (buffer-file-name)
                           ((eq major-mode 'dired-mode)
                            (dired-get-filename nil t))))
                (args (and filename (file-relative-name filename))))
           (read-shell-command "Detached shell command: " nil nil args))))
  (let ((handler (find-file-name-handler (directory-file-name default-directory)
                                         'shell-command)))
    (if handler (funcall handler 'my-detached-shell-command command)
      (call-process shell-file-name nil 0 nil shell-command-switch command))))

;;; Detached command-on-file execution
(defun my-dired--run-detached-shell-command (command)
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
  (cl-flet ((need-confirm-p
             (cmd str)
             (let ((res cmd)
                   (regexp (regexp-quote str)))
               ;; Drop all ? and * surrounded by spaces and `?`.
               (while (and (string-match regexp res)
                           (dired--star-or-qmark-p res str))
                 (setq res (replace-match "" t t res 2)))
               (string-match regexp res))))
    (let* ((on-each (not (dired--star-or-qmark-p command "*" 'keep)))
           (no-subst (not (dired--star-or-qmark-p command "?" 'keep)))
           ;; Get confirmation for wildcards that may have been meant
           ;; to control substitution of a file name or the file name list.
           (ok (cond ((not (or on-each no-subst))
                      (error "You can not combine `*' and `?' substitution marks"))
                     ((need-confirm-p command "*")
                      (y-or-n-p (format-message
                                 "Confirm--do you mean to use `*' as a wildcard? ")))
                     ((need-confirm-p command "?")
                      (y-or-n-p (format-message
                                 "Confirm--do you mean to use `?' as a wildcard? ")))
                     (t))))
      (cond ((not ok) (message "Command canceled"))
            (t
             (if on-each
                 (dired-bunch-files (- 10000 (length command))
                                    (lambda (&rest files)
                                      (my-dired--run-detached-shell-command
                                       (dired-shell-stuff-it command files t arg)))
                                    nil file-list)
               ;; execute the shell command
               (my-dired--run-detached-shell-command
                (dired-shell-stuff-it command file-list nil arg))))))))

(defun my-dired-do-xdg-open (&optional _ file-list)
  "Wrapper around my-dired-do-detached-shell-command that always
uses the xdg-open command."
  (interactive
   (list current-prefix-arg
         (dired-get-marked-files t current-prefix-arg)))
  (dolist (file file-list)
    (let ((bin (or (my-external-programs-get (file-name-extension file))
                   "xdg-open")))
      (my-dired-do-detached-shell-command bin current-prefix-arg (list file)))))

(define-key dired-mode-map (kbd "§") 'my-dired-do-detached-shell-command)
(define-key dired-mode-map (kbd "²") 'my-dired-do-xdg-open)

;;; Jump to last target directory

(defvar my-dired--last-target-dir nil
  "Contains the directory to last copy or move target.")

(defun my-dired--find-last-target-dir (target
                                       name-constructor
                                       fn-list)
  (if (null fn-list)
      target
    (let ((to (file-name-directory (funcall name-constructor (car fn-list)))))
      (if (string= target to)
          (my-dired--find-last-target-dir target name-constructor (cdr fn-list))
        nil))))

(defun my-dired--set-last-target-dir (_file-creator
                                      _operation
                                      fn-list
                                      name-constructor
                                      &optional _marker-char)
  "Advice function for dired-create-files that sets
  `my-dired-set-last-target-dir'."
  (setq my-dired--last-target-dir
        (my-dired--find-last-target-dir
         (file-name-directory (funcall name-constructor (car fn-list)))
         name-constructor
         (cdr fn-list))))

(advice-add 'dired-async-create-files
            :before
            'my-dired--set-last-target-dir)

(advice-add 'dired-create-files
            :before
            'my-dired--set-last-target-dir)

(defun my-dired-jump-to-last-target-dir ()
  (interactive)
  (if (null my-dired--last-target-dir)
      (message "Last target dir not set")
    (dired my-dired--last-target-dir)))

(define-key dired-mode-map (kbd "J") 'my-dired-jump-to-last-target-dir)

;;; Easy async mode toggle
(define-key dired-mode-map (kbd ",") 'dired-async-mode)

;;; Easy wdired mode toggle
(define-key dired-mode-map (kbd ";") 'wdired-change-to-wdired-mode)

;;; Miscellaneous settings
(setq
 ;; Auto-revert
 dired-auto-revert-buffer t
 ;; If two dired windows are open, use the other one as the target for move and
 ;; copy commands.
 dired-dwim-target t
 ;; Human-readable file sizes and literal quoting style
 ;; https://unix.stackexchange.com/questions/258679/why-is-ls-suddenly-wrapping-items-with-spaces-in-single-quotes
 dired-listing-switches "-alhN")

(provide 'my-dired)
