;;; -*- lexical-binding: t; -*-

;;;; FIPLR settings

(require 'fiplr)

(defun my-fiplr-list-files (type path ignored-globs)
  "Like fiplr-list-files, but uses `git ls-files` instead of
`find` for git repositories."
  (if (not (and (eq 'files type)
                (file-exists-p (expand-file-name ".git" path))))
      (fiplr-list-files type path ignored-globs)
    (let ((cmd (concat (format "cd '%s' " path)
                       "&& git ls-files --exclude-standard --other --cached")))
      (split-string (shell-command-to-string cmd)))))

(setq
 fiplr-list-files-function 'my-fiplr-list-files
 fiplr-ignored-globs '((directories (".git"
                                      ".svn"
                                      ".hg"
                                      ".bzr"
                                      ".deps"
                                      ".build"
                                      "target"
                                      "node_modules"))
                        (files (".#*"
                                "*~"
                                "*.so"
                                "*.jpg"
                                "*.png"
                                "*.gif"
                                "*.pdf"
                                "*.gz"
                                "*.zip"
                                ".DS_Store"
                                "*.class"
                                "*.pyc"
                                "*.den"
                                ".elc"))))

;;; Drop an empty .fiplr_root file in a dir for fiplr to consider it a top-level
;;; dir
(add-to-list 'fiplr-root-markers ".fiplr_root" t)

(provide 'my-fiplr)
