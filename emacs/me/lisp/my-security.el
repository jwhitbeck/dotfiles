;;; -*- lexical-binding: t; -*-

;;;; Security settings for emacs.
;;;
;;; Even with these settings, emacs isn't very good at TLS security.
;;;
;;; References:
;;;  - https://www.gnu.org/software/emacs/manual/html_node/emacs-gnutls/Help-For-Users.html
;;;  - https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(require 'gnutls)

(setq
 ;; Ensure that certificates are verified.
 gnutls-verify-error t
 ;; Minimum number of prime bits accepted by GnuTLS for key exchange.
 gnutls-min-prime-bits 2048
 ;; Use cipher suites that enable perfect forward security.
 ;; https://gnutls.org/manual/html_node/Priority-Strings.html
 gnutls-algorithm-priority "PFS")

;;; I can test emacs TLS againt badssl urls using the following snippet.
;; (let ((no-error-urls '()))
;;   (dolist (url '("https://revoked.badssl.com/"  ; As of emacs 26.3, retrieved without error
;;                  "https://pinning-test.badssl.com/" ; As of emacs 26.3, retrieved without error
;;                  "https://expired.badssl.com/"
;;                  "https://wrong.host.badssl.com/"
;;                  "https://untrusted-root.badssl.com/"
;;                  "https://self-signed.badssl.com/"
;;                  "https://invalid-expected-sct.badssl.com/"
;;                  "https://dh1024.badssl.com/"))
;;     (when-let ((buf (ignore-errors (url-retrieve-synchronously url))))
;;       (push url no-error-urls)))
;;   no-error-urls)

(provide 'my-security)
