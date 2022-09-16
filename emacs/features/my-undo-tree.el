;;; -*- lexical-binding: t; -*-

(require 'undo-tree)

;;; Disable minor mode indicator
(setq undo-tree-mode-lighter nil)

;;; Disable undo tree history
(setq undo-tree-auto-save-history nil)

(provide 'my-undo-tree)
