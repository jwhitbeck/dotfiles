;;; -*- lexical-binding: t; -*-

;;; General editing customizations

(require 'my-package)

(my-use-packages undo-tree yasnippet dash)

(custom-set-variables
 '(auto-save-default nil)               ; disable autosave
 '(make-backup-files nil))              ; disable auto backups
(global-auto-revert-mode t)             ; automatically revert a buffer when a file is changed on disk

;;; Highlight and auto-correct whitespace problems
(add-hook 'prog-mode-hook 'my-whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Activate undo tree for text and program editing
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'text-mode-hook 'undo-tree-mode)

(defcustom my-whitespace-styles nil
  "List of (major-mode . whitespace-style) pairs. Used to define custom whitespace-mode styles by major mode."
  :type '(alist :key-type symbol :value-type (list symbol))
  :group 'my-editing)

(defcustom my-default-whitespace-style '(face empty trailing tabs tab-mark)
  "The default whitespace style to use."
  :type '(list symbol)
  :group 'my-editing)

;;; For the ->> macro
(require 'dash)

(defun my-disable-tab-highlighting (mm)
  "Disable tab highlighting for major mode."
  (add-to-list 'my-whitespace-styles
               (cons mm (->> my-default-whitespace-style copy-sequence (delq 'tabs) (delq 'tab-mark)))))

(defun my-whitespace-mode ()
  "Configure whitespace-mode differently depending on the major mode."
  (setq-local whitespace-style (or (cdr (assoc major-mode my-whitespace-styles))
                                   my-default-whitespace-style))
  (whitespace-mode))

;;; No tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;;; Makefile-mode already does that, for example.
(custom-set-variables
 '(indent-tabs-mode nil))

(defun my-enable-indent-tabs ()
  "Enable using tabs for indentation."
  (setq indent-tabs-mode t))

;;; If indent-tabs-mode is off, untabify before saving.
(defun my-untabify-buffer ()
  "Replace all tabs with spaces in buffer."
  (when (not indent-tabs-mode)
    (untabify (point-min) (point-max)))
  nil)

(add-hook 'write-file-hooks 'my-untabify-buffer)

;;; Yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" my-dir))

;;; Lazy-load snippets
(defvar my-are-snippets-loaded nil
  "t if the snippets have been loaded.")

(defun my-yas-minor-mode ()
  "Like yas-minor-mode but loads snippets if that hasn't already been done."
  (when (not my-are-snippets-loaded)
    (yas-reload-all)
    (setq my-are-snippets-loaded t))
  (yas-minor-mode))

(provide 'my-editing)
