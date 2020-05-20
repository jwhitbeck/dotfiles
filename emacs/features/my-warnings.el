;;; -*- lexical-binding: t; -*-

;;;; Suppress warnings
(require 'warnings)

;;; Don't warn if undo info exceeds `undo-outer-limit'.
(add-to-list 'warning-suppress-types '(undo discard-info))

(provide 'my-warnings)
