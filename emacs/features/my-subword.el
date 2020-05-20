;;; -*- lexical-binding: t; -*-

(require 'subword)
(require 'my-mode-line)

;;; Disable minor mode indicator
(my-mode-line-hide-minor-mode 'subword-mode)

(provide 'my-subword)
