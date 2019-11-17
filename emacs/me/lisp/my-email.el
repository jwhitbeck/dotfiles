;;; -*- lexical-binding: t; -*-

;;; XXX out of global config
;;; Mu4e customizations

(autoload 'my-start-mu4e "my-mu4e"
  "Starts mu4e with appropriate settings depending on whether or
  not we have an internet connection. If called with a prefix
  arg, always use offline settings."
  t)

(autoload 'my-go-to-inbox "my-mu4e"
  "Open mu4e inbox, taking care of starting mu4e if necessary. If
called with a prefix and mu4e is not yet running, always use
offline settings."
  t)

(autoload 'my-compose-email "my-mu4e"
  "Start composing a mail message to send. If mu4e is not yet
running, start it in the background."
  t)

(global-set-key (kbd "C-c m") 'my-start-mu4e)
(global-set-key (kbd "C-c i") 'my-go-to-inbox)
(global-set-key (kbd "C-x m") 'my-compose-email)

(provide 'my-email)
