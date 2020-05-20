;;; -*- lexical-binding: t; -*-

;;;; Time helper functions

;;;###autoload
(defmacro my-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;;;###autoload
(defun my-current-utc-time ()
  "Returns an ISO 8061 UTC timestamp. Prints it to the echo area
if called interactively."
  (interactive)
  (set-time-zone-rule t)
  (let ((timestamp (prog1 (format-time-string "%Y-%m-%d-T%TZ")
                     (set-time-zone-rule nil))))
    (if (called-interactively-p 'interactive)
        (message "%s" timestamp)
      timestamp)))

(provide 'my-time)
