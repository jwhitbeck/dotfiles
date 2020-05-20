;;; -*- lexical-binding: t; -*-

;;;; Custom Org Babel Clojure settings

(require 'cider)
(require 'ob-clojure)

;;; Use cider for org-babel Org-babel
(setq org-babel-clojure-backend 'cider)

(provide 'my-ob-clojure)
