;;; -*- lexical-binding: t; -*-

;;; Handle ANSI escape codes for cursors and erasures.
;;;
;;; https://en.wikipedia.org/wiki/ANSI_escape_code
;;;
;;; TODO: Support more codes.

(require 'comint)

(defvar my-ansi-cursor--control-seq-regexp
  ansi-color-control-seq-regexp
  "Regexp matching an ANSI control sequence.")

(defvar my-ansi-cursor--control-param-regexp
  "\\([0-9]*\\)[ABCDEFK;]"
  "Regexp that matches cursor and erasure control sequence parameters.")

(defun my-ansi-cursor--preoutput-filter (string)
  (let ((pos 0))
    (while (string-match my-ansi-cursor--control-seq-regexp string pos)
      (setq pos (match-end 0))
      (let ((seq (match-string 0 string)))
        (if (string-match my-ansi-cursor--control-param-regexp seq)
            (let ((n (string-to-number (match-string 1 seq)))
                  (action (aref seq (match-end 1))))
              (pcase action
                ;; Cursor previous line
                (?F (progn
                      (goto-char (point-at-bol))
                      (kill-line (- (max 1 n)))))
                (_ nil)))))))
  string)

(provide 'my-ansi-cursor)
