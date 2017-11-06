;;; -*- lexical-binding: t; -*-

;;; Emacs performance tweaks

;;; Reduce occurence of garbage collection
(custom-set-variables '(gc-cons-threshold 20000000))

;;; Increase limit of lisp variable bindings
(custom-set-variables '(max-specpdl-size 2500))

(provide 'my-performance)
