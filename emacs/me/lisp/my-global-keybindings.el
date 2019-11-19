;;; -*- lexical-binding: t; -*-

;;;; Global keybindings

;;; Insert special characters using f7
(defvar my-f7-chars
  '(;; EM dash. See https://www.thepunctuationguide.com/em-dash.html for usage.
    ("_" . "—")
    ;; EN dash. See https://www.thepunctuationguide.com/en-dash.html for usage.
    ("-" . "–")))

(dolist (e my-f7-chars)
  (global-set-key (kbd (concat "<f7> " (car e)))
                  (lambda () (interactive) (insert (cdr e)))))

;;; Unfill paragraph
(global-set-key (kbd "M-Q") 'my-unfill-paragraph)

;;; Set M-- and M-+ to decrease/increase the font size
(defun my-text-scale-reset ()
  "Reset the height of the default face in the current buffer."
  (interactive)
  (text-scale-set 0))

(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-0") 'my-text-scale-reset)

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

;;; IDO for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; Fuzzy project file finding
(global-set-key (kbd "C-x p") 'fiplr-find-file)

;;; Better buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; git-grep keybinding
(global-set-key (kbd "C-x g") 'vc-git-grep)

;;; Print current UTC time in echo area
(global-set-key (kbd "C-c u") 'my-current-utc-time)

;;; Quickly connect to remote hosts
(global-set-key (kbd "C-c s") 'my-remote-shell)

;;; Dired fast access
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;;; Fast shell access
(global-set-key (kbd "C-x x") 'shell)

;;; Org-mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'my-global-keybindings)