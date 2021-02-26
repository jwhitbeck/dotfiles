;;; -*- lexical-binding: t; -*-

;;;; Shell configuration
(require 'comint)
(require 'my-ansi-cursor)

;;; Handle ANSI cursor escape codes.
(add-hook 'comint-preoutput-filter-functions
          'my-ansi-cursor--preoutput-filter)

(provide 'my-shell)
