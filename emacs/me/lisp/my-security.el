;;; -*- lexical-binding: t; -*-

;;; Security settings for emacs. Should be the first feature to load.

;;; TLS Ensure that certificates are verified. See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
;;; for full explanation.
(require 'gnutls)
(custom-set-variables
 '(gnutls-verify-error t)
 '(gnutls-min-prime-bits 2048)
 '(gnutls-algorithm-priority "PFS"))

;;; Even with these settings, emacs isn't very good at TLS security.
;;; See https://news.ycombinator.com/item?id=17567347 for a more detailed discussion.
;;;
;;; I can test emacs TLS againt badssl urls using the following snippet.
;; (mapcar (lambda (host)
;;           (ignore-errors (url-retrieve-synchronously host)))
;;         '("https://revoked.badssl.com/"
;;           "https://pinning-test.badssl.com/"
;;           "https://invalid-expected-sct.badssl.com/"))

(provide 'my-security)
