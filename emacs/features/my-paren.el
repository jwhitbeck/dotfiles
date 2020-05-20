;;; -*- lexical-binding: t; -*-

;;; Automatically show opened/closed parentheses

(require 'paren)

;;; show-paren is a global minor mode
(show-paren-mode)

(setq
 ;; Immediately show matching parentheses.
 show-paren-delay 0
 ;; Highlight full expression contained between parentheses.
 show-paren-style 'parenthesis)

(provide 'my-paren)
