;;; -*- lexical-binding: t; -*-

;;; Elisp

(require 'my-package)
(require 'my-editing)
(my-use-packages auto-indent-mode paredit)

(my-disable-tab-highlighting 'emacs-lisp-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; minibuffer
(defun my-conditionally-enable-paredit-mode ()
  "Enable paredit-mode during eval-expression."
  (when (eq this-command 'eval-expression)
    (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'my-conditionally-enable-paredit-mode)

;;; Some convenience elisp functions
(defun constantly (x)
  "Return a function that always returns x."
  (lambda () x))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;;; Generate TAGS for all elisp files on load path
(defvar my-elisp-tags-file (expand-file-name "me/TAGS.elisp" user-emacs-directory))

(add-to-list 'tags-table-list my-elisp-tags-file)

(defun my-generate-elisp-tags ()
  "Rebuilds the ~/.emacs.d/me/elisp_tags file from all elisp files on the load path."
  (interactive)
  (when (file-exists-p my-elisp-tags-file)
    (delete-file my-elisp-tags-file))
  (dolist (dir load-path)
    (dolist (file (directory-files dir))
      (when (and (or (string-suffix-p ".el" file)
                     (string-suffix-p ".el.gz" file))
                 (not (string-prefix-p "." file)))
        (let ((elisp-file (concat (file-name-as-directory dir) file)))
          (call-process "etags" nil t t "--append" "--output" my-elisp-tags-file elisp-file))))))

;;; If no elisp-tags file exists, create it.
(when (not (file-exists-p my-elisp-tags-file))
  (message "Regenerating elisp tags")
  (my-generate-elisp-tags))

(provide 'my-elisp)
