;;; -*- lexical-binding: t; -*-

;;;; Emacs performance tweaks

(setq
 ;; Reduce occurence of garbage collection
 gc-cons-threshold 20000000
 ;; Increase limit of lisp variable bindings
 max-specpdl-size 2500)

(provide 'my-performance)
