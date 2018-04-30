;;; -*- lexical-binding: t; -*-

;;; Security settings for emacs. Should be the first feature to load.

;;; TLS Ensure that certificates are verified. See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
;;; for full explanation.
(require 'gnutls)
(custom-set-variables
 '(gnutls-verify-error t)
 '(gnutls-algorithm-priority "PFS"))

(provide 'my-security)
