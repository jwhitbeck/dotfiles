;;; -*- lexical-binding: t; -*-

;;;; Java mode settings

;;; Fix indentation to be compatible with Google style guide
(c-add-style "java-google" '("user"
                             (c-basic-offset . 2)
                             (c-offsets-alist . ((case-label . +)
                                                 (arglist-intro . ++)
                                                 (statement-cont . ++)))))

(defun my-java--enable-google-style ()
  "Use Google Java coding style in buffer."
  (set-fill-column 100)
  (c-set-style "java-google"))

(add-hook 'java-mode-hook 'my-java--enable-google-style)
(add-hook 'java-mode-hook 'my-whitespace-mode-default)

(add-hook 'java-mode-hook 'my-java-eclim--enable-if-appropriate)

;;; Provide the start-eclimd command at startup
;(autoload 'start-eclimd "eclimd" "Start the eclim daemon." t)

(provide 'my-java-mode)
