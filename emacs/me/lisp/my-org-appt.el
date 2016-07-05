;;; -*- lexical-binding: t; -*-

;;; Appointments integration

(eval-when-compile
  (require 'org)
  (require 'org-agenda)
  (require 'appt))

(require 'my-commands)

(appt-activate 't)

(defun my-update-appointments-on-agenda-save ()
  (when (member (buffer-file-name) org-agenda-files)
    (org-agenda-to-appt 't)))
(add-hook 'after-save-hook 'my-update-appointments-on-agenda-save)

(defun my-appt-notify (time-to-appt time msg)
  (my-popup (format "Appointment in %s minutes" time-to-appt)
            msg
            "/usr/share/icons/gnome/256x256/status/appointment-soon.png"
            "/usr/share/sounds/ubuntu/stereo/message.ogg"))

(custom-set-variables
 '(appt-display-interval (+ 1 appt-message-warning-time)) ; no duplicate reminders
 '(app-display-mode-line nil) ; disable mode-line reminders
 '(appt-disp-window-function 'my-appt-notify))

(provide 'my-org-appt)
