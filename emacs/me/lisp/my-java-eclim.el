;;; -*- lexical-binding: t; -*-

;;;; Eclipse integration for java

(require 'eclim)
(require 'eclimd)

;;; Company mode eclim integration
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

;;; Copied from `global-eclim-mode'
;;;###autoload
(defun my-enable-eclim-if-appropriate ()
  (if (and buffer-file-name
           (eclim--accepted-p buffer-file-name)
           (eclim--project-dir))
      (eclim-mode 1)))

;;; Re-bind the find-tags command to eclipse's jump to declaration
(define-key eclim-mode-map (kbd "M-.") 'eclim-java-find-declaration)

(provide 'my-java-eclim)
