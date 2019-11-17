;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "my-dired" "my-dired.el" (0 0 0 0))
;;; Generated autoloads from my-dired.el

(autoload 'my-midnight-commander "my-dired" "\
Creates two side-by-side dired buffers, in a
midnight-commander style, where the one on the right is the
default destination for dired operation on the left buffer.

\(fn SRC DEST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-dired" '(#("my-" 0 3 (face font-lock-function-name-face fontified nil)))))

;;;***

;;;### (autoloads nil "my-dirs" "my-dirs.el" (0 0 0 0))
;;; Generated autoloads from my-dirs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-dirs" '(#("my-" 0 3 (face font-lock-variable-name-face fontified nil)))))

;;;***

;;;### (autoloads nil "my-editing" "my-editing.el" (0 0 0 0))
;;; Generated autoloads from my-editing.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-editing" '("my-f7-chars")))

;;;***

;;;### (autoloads nil "my-elisp-mode" "my-elisp-mode.el" (0 0 0 0))
;;; Generated autoloads from my-elisp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-elisp-mode" '("my-disable-emacs-lisp-checkdoc")))

;;;***

;;;### (autoloads nil "my-external" "my-external.el" (0 0 0 0))
;;; Generated autoloads from my-external.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-external" '(#("my-" 0 3 (fontified nil face font-lock-variable-name-face)))))

;;;***

;;;### (autoloads nil "my-fill-column-indicator" "my-fill-column-indicator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-fill-column-indicator.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-fill-column-indicator" '("company-")))

;;;***

;;;### (autoloads nil "my-fiplr" "my-fiplr.el" (0 0 0 0))
;;; Generated autoloads from my-fiplr.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-fiplr" '("my-fiplr-list-files")))

;;;***

;;;### (autoloads nil "my-java-eclim" "my-java-eclim.el" (0 0 0 0))
;;; Generated autoloads from my-java-eclim.el

(autoload 'my-enable-eclim-if-appropriate "my-java-eclim" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "my-java-mode" "my-java-mode.el" (0 0 0 0))
;;; Generated autoloads from my-java-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-java-mode" '("my-enable-java-google-style")))

;;;***

;;;### (autoloads nil "my-markdown-mode" "my-markdown-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from my-markdown-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-markdown-mode" '("my-enable-orgtbl-mode")))

;;;***

;;;### (autoloads nil "my-minibuffer" "my-minibuffer.el" (0 0 0 0))
;;; Generated autoloads from my-minibuffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-minibuffer" '(#("my-conditionally-enable-paredit-mode" 0 36 (face font-lock-function-name-face fontified t)))))

;;;***

;;;### (autoloads nil "my-mu4e" "my-mu4e.el" (0 0 0 0))
;;; Generated autoloads from my-mu4e.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-mu4e" '("my-")))

;;;***

;;;### (autoloads nil "my-notifications" "my-notifications.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from my-notifications.el

(autoload 'my-popup "my-notifications" "\
Show a popup if we're on X, or echo it otherwise; TITLE is the
title of the message, MSG is the context. Optionally, you can
provide an ICON and SOUND.

\(fn TITLE MSG &optional ICON SOUND)" t nil)

;;;***

;;;### (autoloads nil "my-org" "my-org.el" (0 0 0 0))
;;; Generated autoloads from my-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-org" '(#("my-org-t" 0 8 (fontified nil face font-lock-function-name-face)))))

;;;***

;;;### (autoloads nil "my-org-html" "my-org-html.el" (0 0 0 0))
;;; Generated autoloads from my-org-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-org-html" '(#("fci-mode-override-advice" 0 1 (face font-lock-function-name-face fontified nil) 1 24 (face font-lock-function-name-face fontified nil)) #("my-" 0 3 (fontified nil face font-lock-variable-name-face)))))

;;;***

;;;### (autoloads nil "my-package" "my-package.el" (0 0 0 0))
;;; Generated autoloads from my-package.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-package" '("my-")))

;;;***

;;;### (autoloads nil "my-packages" "my-packages.el" (0 0 0 0))
;;; Generated autoloads from my-packages.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-packages" '("my-packages")))

;;;***

;;;### (autoloads nil "my-remote-shell" "my-remote-shell.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from my-remote-shell.el

(autoload 'my-remote-shell-at-point "my-remote-shell" "\
Open a remote shell on the host-name under point.

\(fn)" t nil)

(autoload 'my-tramp-connection-history "my-remote-shell" "\
Return the list of all user@hostname pairs present in the
tramp connection history.

\(fn)" nil nil)

(defvar my-list-remote-hosts-function 'my-tramp-connection-history "\
A function that returns the list of hosts to consider for
  `my-remote-shell'.")

(autoload 'my-remote-shell "my-remote-shell" "\
Open a tramp-enabled shell on HOSTNAME.

\(fn HOSTNAME)" t nil)

;;;***

;;;### (autoloads nil "my-string" "my-string.el" (0 0 0 0))
;;; Generated autoloads from my-string.el

(autoload 'my-replace-string "my-string" "\
Replace occurrences of 'old' string within string 's' with
  'new' string.

\(fn S OLD NEW)" nil nil)

(autoload 'my-replace-string-re "my-string" "\
Replace matches of regexp within string 's' with 'new'
  string.

\(fn S RE NEW)" nil nil)

;;;***

;;;### (autoloads nil "my-tabs" "my-tabs.el" (0 0 0 0))
;;; Generated autoloads from my-tabs.el

(autoload 'my-untabify-buffer "my-tabs" "\
Replace all tabs with spaces in buffer.

\(fn)" nil nil)

(autoload 'my-enable-indent-tabs "my-tabs" "\
Enable using tabs for indentation. Intended for use in hooks.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "my-time" "my-time.el" (0 0 0 0))
;;; Generated autoloads from my-time.el

(autoload 'my-time "my-time" "\
Measure the time it takes to evaluate BODY.

\(fn &rest BODY)" nil t)

(autoload 'my-current-utc-time "my-time" "\
Returns an ISO 8061 UTC timestamp. Prints it to the echo area
if called interactively.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "my-ui" "my-ui.el" (0 0 0 0))
;;; Generated autoloads from my-ui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-ui" '("my-")))

;;;***

;;;### (autoloads nil "my-whitespace" "my-whitespace.el" (0 0 0 0))
;;; Generated autoloads from my-whitespace.el

(autoload 'my-whitespace-mode-default "my-whitespace" "\


\(fn)" nil nil)

(autoload 'my-whitespace-mode-tabs "my-whitespace" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-whitespace" '(#("my-whitespace-mode--enable" 0 26 (face font-lock-function-name-face fontified nil)))))

;;;***

;;;### (autoloads nil "my-wiki" "my-wiki.el" (0 0 0 0))
;;; Generated autoloads from my-wiki.el

(autoload 'my-browse-wikimedia "my-wiki" "\


\(fn SITE WORD PROMPT-LANGUAGE\\=\\?)" nil nil)

(autoload 'my-loopkup-wikipedia "my-wiki" "\


\(fn WORD &optional PROMPT-LANGUAGE\\=\\?)" t nil)

(autoload 'my-loopkup-wiktionary "my-wiki" "\


\(fn WORD &optional PROMPT-LANGUAGE\\=\\?)" t nil)

;;;***

;;;### (autoloads nil "my-windows" "my-windows.el" (0 0 0 0))
;;; Generated autoloads from my-windows.el

(autoload 'my-rotate-window-split-horizontal "my-windows" "\
Rotate windows from a 2-window vertical split to a 2-window horizontal split.

\(fn)" t nil)

(autoload 'my-rotate-window-split-vertical "my-windows" "\
Rotate windows from a 2-window horizontal split to a 2-window vertical split.

\(fn)" t nil)

(autoload 'my-window-switch "my-windows" "\
Switch the buffers displayed in a 2-window split.

\(fn)" t nil)

(autoload 'my-split-window-below "my-windows" "\
Split the window vertically and move focus to the new window
below the selected one.

\(fn)" t nil)

(autoload 'my-split-window-right "my-windows" "\
Split the window horizontal and move focus to the new window
to the right of the selected one.

\(fn)" t nil)

(autoload 'my-toggle-dedicated-window "my-windows" "\
Toggle whether or not the window is dedicated to its buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-windows" '(#("my-set-buffer-name-face" 0 23 (face font-lock-function-name-face fontified nil)))))

;;;***

;;;### (autoloads nil nil ("my-ace-window.el" "my-clojure.el" "my-css-mode.el"
;;;;;;  "my-email.el" "my-ess-mode.el" "my-go-mode.el" "my-locale.el"
;;;;;;  "my-magit.el" "my-make-mode.el" "my-performance.el" "my-prog-modes.el"
;;;;;;  "my-scheme.el" "my-security.el" "my-shell.el" "my-sql.el"
;;;;;;  "my-text-mode.el" "my-tramp.el" "my-vc.el" "my-yaml-mode.el"
;;;;;;  "my-yasnippet.el") (0 0 0 0))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
