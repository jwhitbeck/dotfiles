;;; -*- lexical-binding: t; -*-

;;;; Desktop notifications

;;; Assumes that emacs is running on ubuntu
;;; Adapted from http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
;;;###autoload
(defun my-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the
title of the message, MSG is the context. Optionally, you can
provide an ICON and SOUND."
  (interactive
   (list (read-from-minibuffer "Title: ")
         (read-from-minibuffer "Message: ")))
  (when sound
    (call-process "pacmd" nil 0 nil "play-file" sound "0"))
  (if (eq window-system 'x)
      (let ((args (if icon (list "-i" icon title msg) (list title msg))))
        (apply 'call-process "notify-send" nil 0 nil args))
    ;; text only version
    (message (format "%s: %s" title msg))))

(provide 'my-notifications)
