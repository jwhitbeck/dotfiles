;;; -*- lexical-binding: t; -*-

;;;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(provide 'my-uniquify)
