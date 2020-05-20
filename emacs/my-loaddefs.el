;;; my-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "init" "init.el" (0 0 0 0))
;;; Generated autoloads from init.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "init" '("my-")))

;;;***

;;;### (autoloads nil "my-external-programs" "my-external-programs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-external-programs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-external-programs" '(#("my-external-programs" 0 20 (face font-lock-variable-name-face fontified nil)))))

;;;***

;;;### (autoloads nil "my-mode-line" "my-mode-line.el" (0 0 0 0))
;;; Generated autoloads from my-mode-line.el

(autoload 'my-mode-line-hide-minor-mode "my-mode-line" "\
Hide the minor mode from the mode line. Useful to hide
  globally enabled minor modes.

\(fn MODE)" nil nil)

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

;;;### (autoloads nil "my-packages" "my-packages.el" (0 0 0 0))
;;; Generated autoloads from my-packages.el

(autoload 'my-packages-install "my-packages" "\
Ensures the packages are installed. Refreshes package list if
  necessary.

\(fn PKG)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-packages" '("my-packages")))

;;;***

;;;### (autoloads nil "my-remote-shell" "my-remote-shell.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from my-remote-shell.el

(defvar my-remote-shell-list-hosts-functions '(my-tramp-connection-history) "\
A list of functions that return lists of hosts to consider for
  `my-remote-shell'.")

(autoload 'my-remote-shell "my-remote-shell" "\
Open a tramp-enabled shell on HOSTNAME.

\(fn HOSTNAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-remote-shell" '("my-")))

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

;;;### (autoloads nil "my-whitespace" "my-whitespace.el" (0 0 0 0))
;;; Generated autoloads from my-whitespace.el

(autoload 'my-whitespace-mode "my-whitespace" "\
Like whitespace-mode, but intelligently decides whether or not
to highlight tabs.

\(fn)" nil nil)

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

(autoload 'my-windows-rotate-split-horizontal "my-windows" "\
Rotate windows from a 2-window vertical split to a 2-window horizontal split.

\(fn)" t nil)

(autoload 'my-windows-rotate-split-vertical "my-windows" "\
Rotate windows from a 2-window horizontal split to a 2-window vertical split.

\(fn)" t nil)

(autoload 'my-windows-switch "my-windows" "\
Switch the buffers displayed in a 2-window split.

\(fn)" t nil)

(autoload 'my-windows-split-below "my-windows" "\
Split the window vertically and move focus to the new window
below the selected one.

\(fn)" t nil)

(autoload 'my-windows-split-right "my-windows" "\
Split the window horizontal and move focus to the new window
to the right of the selected one.

\(fn)" t nil)

(autoload 'my-windows-toggle-dedicated "my-windows" "\
Toggle whether or not the window is dedicated to its buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-windows" '("my-windows--set-buffer-name-face")))

;;;***

;;;### (autoloads nil "my-word-wrap" "my-word-wrap.el" (0 0 0 0))
;;; Generated autoloads from my-word-wrap.el

(autoload 'my-word-wrap "my-word-wrap" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-word-wrap" '("my-word-wrap--state")))

;;;***

;;;### (autoloads nil nil ("my-global-keybindings.el" "my-yasnippet.el")
;;;;;;  (0 0 0 0))

;;;***

(provide 'my-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-loaddefs.el ends here
