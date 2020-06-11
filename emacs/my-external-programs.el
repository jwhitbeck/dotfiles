;;; -*- lexical-binding: t; -*-

;;;; Preferred external programs

(defvar my-external-programs--alist nil
  "Alist mapping an extension to the external program that should open it.")

(defvar my-external-programs-hooks nil
  "A list of functions, run after setting the external programs,
  that take `my-external-programs--alist' as an argument.")

(defun my-external-programs-get (extension)
  "Return the program used to open file of the given extension."
  (cdr (assoc extension my-external-programs--alist)))

(defun my-external-programs-set (alist)
  "Sets the mapping of extension to program, and runs the hooks."
  (setq my-external-programs--alist alist)
  (dolist (hook my-external-programs-hooks)
    (funcall hook my-external-programs--alist)))

(provide 'my-external-programs)
