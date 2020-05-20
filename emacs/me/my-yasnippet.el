;;; -*- lexical-binding: t; -*-

;;;; Yasnippet settings

(require 'yasnippet)
(require 'my-dirs)

(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" my-dir))

;;; Snippets must be reloaded at startup time.
(yas-reload-all)

(provide 'my-yasnippet)
