;;; -*- lexical-binding: t; -*-

;;;; Global keybindings

;;; References:
;;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

;;; Insert special characters using f7
(global-set-key
 (kbd "<f7>")
 '(keymap
   ;; EM dash. See https://www.thepunctuationguide.com/em-dash.html for usage.
   (?_ . "—")
   ;; EN dash. See https://www.thepunctuationguide.com/en-dash.html for usage.
   (?- . "–")))

;;; Unfill paragraph
(global-set-key (kbd "C-c q") 'my-unfill-paragraph)

;;; Set M-- and M-+ to decrease/increase the font size
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-0") 'text-scale-mode)

;;; Word wrapping
(global-set-key (kbd "M-à") 'my-word-wrap)

;;; Ace-jump bindings
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;; IMenu
(global-set-key (kbd "C-²") 'imenu)

;;; Window navigation
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-o") 'ace-window) ; Convenience binding for typing C-x o too quickly

;;; Use single key presses for winner mode
(global-set-key (kbd "<f8>") 'winner-undo)
(global-set-key (kbd "<f9>") 'winner-redo)

;;; Window management
(global-set-key (kbd "C-x |") 'my-rotate-window-split-horizontal)
(global-set-key (kbd "C-x _") 'my-rotate-window-split-vertical)
(global-set-key (kbd "C-x ç") 'my-window-switch)

;;; Simplify window management for french keyboards
(global-set-key (kbd "C-x à") 'delete-window) ; C-x 0
(global-set-key (kbd "C-x &") 'delete-other-windows) ; C-x 1
(global-set-key (kbd "C-x é") 'my-split-window-below) ; C-x 2
(global-set-key (kbd "C-x \"") 'my-split-window-right) ; C-x 3
(global-set-key (kbd "C-x ²") 'my-toggle-dedicated-window)

;;; TAB autocompletes
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)

;;; Use swiper for C-s
(global-set-key (kbd "C-s") 'swiper)

;;; Fuzzy project file finding
(global-set-key (kbd "C-c p") 'projectile-command-map)

;;; Better buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Print current UTC time in echo area
(global-set-key (kbd "C-c u") 'my-current-utc-time)

;;; Fast shell access
(global-set-key (kbd "<f5>") 'my-shell)
(global-set-key (kbd "C-c s") 'my-shell-switchb)

;;; Quickly connect to remote hosts
(global-set-key (kbd "C-c r") 'my-remote-shell)

;;; Dired fast access
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;;; Org-mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'my-global-keybindings)
