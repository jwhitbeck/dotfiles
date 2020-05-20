;;; -*- lexical-binding: t; -*-

;;;; Winner mode saves the history of window

(require 'winner)

;;; Don't bind winner-mode keys as we are going to define our own.
(setq winner-dont-bind-my-keys t)

(winner-mode 1)

(provide 'my-winner)
