;;; -*- lexical-binding: t; -*-

;;;; Ange-ftp settings

(require 'ange-ftp)

(setq
 ;; Always use passive mode for data transfers. Ange-ftp has race conditions if
 ;; run in active mode.
 ange-ftp-try-passive-mode t
 ;; Connect as anonymous unless explicitly providing a username.
 ange-ftp-default-user "anonymous")

(provide 'my-ange-ftp)
