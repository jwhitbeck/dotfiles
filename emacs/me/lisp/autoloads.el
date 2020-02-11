;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "my-dired" "my-dired.el" (0 0 0 0))
;;; Generated autoloads from my-dired.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-dired" '("my-")))

;;;***

;;;### (autoloads nil "my-dirs" "my-dirs.el" (0 0 0 0))
;;; Generated autoloads from my-dirs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-dirs" '(#("my-" 0 3 (face font-lock-variable-name-face fontified nil)))))

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

(autoload 'my-unfill-paragraph "my-fill-column-indicator" "\


\(fn)" t nil)

(autoload 'my-unfill-region "my-fill-column-indicator" "\


\(fn START END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-fill-column-indicator" '(#("company-" 0 8 (fontified t face font-lock-function-name-face)))))

;;;***

;;;### (autoloads nil "my-git-commit" "my-git-commit.el" (0 0 0 0))
;;; Generated autoloads from my-git-commit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-git-commit" '("my-git-commit-mode-fill-column")))

;;;***

;;;### (autoloads nil "my-global-keybindings" "my-global-keybindings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-global-keybindings.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-global-keybindings" '(#("my-" 0 3 (fontified nil face font-lock-function-name-face)))))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-markdown-mode" '(#("my-enable-orgtbl-mode" 0 21 (face font-lock-function-name-face fontified nil)))))

;;;***

;;;### (autoloads nil "my-minibuffer" "my-minibuffer.el" (0 0 0 0))
;;; Generated autoloads from my-minibuffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-minibuffer" '(#("my-conditionally-enable-paredit-mode" 0 36 (face font-lock-function-name-face fontified t)))))

;;;***

;;;### (autoloads nil "my-mode-line" "my-mode-line.el" (0 0 0 0))
;;; Generated autoloads from my-mode-line.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-mode-line" '("my-hide-minor-mode")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-org" '("my-org-t")))

;;;***

;;;### (autoloads nil "my-org-html" "my-org-html.el" (0 0 0 0))
;;; Generated autoloads from my-org-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-org-html" '(#("fci-mode-override-advice" 0 1 (face font-lock-function-name-face fontified nil) 1 24 (face font-lock-function-name-face fontified nil)) #("my-" 0 3 (fontified nil face font-lock-variable-name-face)))))

;;;***

;;;### (autoloads nil "my-packages" "my-packages.el" (0 0 0 0))
;;; Generated autoloads from my-packages.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-packages" '("my-packages")))

;;;***

;;;### (autoloads nil "my-projectile" "my-projectile.el" (0 0 0 0))
;;; Generated autoloads from my-projectile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-projectile" '(#("?x" 0 2 (fontified t)))))

;;;***

;;;### (autoloads nil "my-remote-shell" "my-remote-shell.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from my-remote-shell.el

(autoload 'my-tramp-connection-history "my-remote-shell" "\
Return the list of all user@hostname pairs present in the
tramp connection history.

\(fn)" nil nil)

(defvar my-remote-shell-list-hosts-functions '(my-tramp-connection-history) "\
A list of functions that return lists of hosts to consider for
  `my-remote-shell'.")

(autoload 'my-remote-shell "my-remote-shell" "\
Open a tramp-enabled shell on HOSTNAME.

\(fn HOSTNAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-remote-shell" '(#("my-remote-shell-list-hosts" 0 26 (face font-lock-function-name-face fontified t)))))

;;;***

;;;### (autoloads nil "my-shell" "my-shell.el" (0 0 0 0))
;;; Generated autoloads from my-shell.el

(autoload 'my-shell-switchb "my-shell" "\


\(fn)" t nil)

(autoload 'my-shell "my-shell" "\
Like `shell' but always opens the *shell* buffer in the
current window. With a C-u prefix, opens a new shell. With C-u
C-u prefix prompts for the new buffer's name. If the buffer
doesn't exist and no prefix arg is given, create it in the home
directory.

\(fn &optional ARG)" t nil)

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-ui" '(#("my-disable-line-wrap" 0 20 (face font-lock-function-name-face fontified nil)))))

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

;;;### (autoloads nil "my-word-wrap" "my-word-wrap.el" (0 0 0 0))
;;; Generated autoloads from my-word-wrap.el

(autoload 'my-word-wrap "my-word-wrap" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-word-wrap" '("my-word-wrap-state")))

;;;***

;;;### (autoloads nil nil ("my-ace-window.el" "my-clojure-mode.el"
;;;;;;  "my-css-mode.el" "my-editing.el" "my-ess-mode.el" "my-file-associations.el"
;;;;;;  "my-flyspell.el" "my-go-mode.el" "my-ivy.el" "my-locale.el"
;;;;;;  "my-magit.el" "my-make-mode.el" "my-nov.el" "my-ob-clojure.el"
;;;;;;  "my-ox.el" "my-performance.el" "my-prog-modes.el" "my-scheme.el"
;;;;;;  "my-security.el" "my-sh-mode.el" "my-sql.el" "my-text-mode.el"
;;;;;;  "my-tramp.el" "my-yaml-mode.el" "my-yasnippet.el") (0 0 0
;;;;;;  0))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
