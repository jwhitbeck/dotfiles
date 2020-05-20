;;; -*- lexical-binding: t; -*-

;;; Shell mode is built on top of comint mode

(require 'comint)

(setq
 comint-scroll-to-bottom-on-input t   ; Always insert at the bottom.
 comint-move-point-for-output nil     ; Always add output at the bottom.
 comint-scroll-show-maximum-output t  ; Scroll to show max possible output.
 comint-completion-autolist t         ; Show completion list when ambiguous.
 comint-input-ignoredups t            ; No duplicates in command history.
 comint-completion-addsuffix t        ; Insert space/slash after file completion.
 comint-get-old-input (lambda () "")  ; What gets sent to prompt when pressing enter in the buffer.
 comint-buffer-maximum-size 20000     ; Max length of buffer in lines.
 comint-input-ring-size 5000          ; Max shell history size.
 )

;; Do not use `less` as the default pager.
(setenv "PAGER" "cat")

;; Truncate buffers continuously.
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; Disable C-c SPC binding in comint mode as it overrides the ace-window-mode
;; key binding and provides functionality that is redundant with C-j.
(define-key comint-mode-map  (kbd "C-c SPC") nil)

(provide 'my-comint)
