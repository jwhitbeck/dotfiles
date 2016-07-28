;;; -*- lexical-binding: t; -*-

;;; Shell mode customization

(require 'my-elisp)

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)    ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t)   ; scroll to show max possible output
 '(comint-completion-autolist t)          ; show completion list when ambiguous
 '(comint-input-ignoredups t)             ; no duplicates in command history
 '(comint-completion-addsuffix t)         ; insert space/slash after file completion
 '(comint-get-old-input (constantly ""))  ; what gets sent to prompt when pressing enter in the buffer
 '(comint-buffer-maximum-size 20000)      ; max length of buffer in lines
 '(comint-input-ring-size 5000)           ; max shell history size
 '(explicit-shell-file-name "/bin/bash")) ; Always use bash on remote hosts
(setenv "PAGER" "cat")                                             ; Do not use `less` as the default pager
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; truncate buffers continuously

(provide 'my-shell)