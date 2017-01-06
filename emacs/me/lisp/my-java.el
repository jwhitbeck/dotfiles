;;; -*- lexical-binding: t; -*-

;;; Java mode settings

(require 'my-package)
(my-use-packages eclim company company-emacs-eclim)

(eval-when-compile
  (require 'my-java-eclim))

;;; Fix indentation to be compatible with Google style guide
(c-add-style "java-google" '("user" (c-offsets-alist . ((case-label . +)
                                                        (arglist-intro . ++)
                                                        (statement-cont . ++)))))

(defun my-enable-java-google-style ()
  "Use Google Java coding style in buffer."
  (set-fill-column 100)
  (c-set-style "java-google"))

(add-hook 'java-mode-hook 'my-enable-java-google-style)

;;; Defer loading eclipse integration until we open a java file
(autoload 'my-enable-eclim-if-appropriate "my-java-eclim")

(defun my-eclim-mode ()
  (my-enable-eclim-if-appropriate))

(add-hook 'java-mode-hook 'my-eclim-mode)

;;; Provide the start-eclimd command at startup
(autoload 'start-eclimd "eclimd" "Start the eclim daemon." t)

(provide 'my-java)
