;;; -*- lexical-binding: t; -*-

;;;; Projectile settings

(require 'projectile)
(setq
 ;; Use git-grep rather than grep for git projects
 projectile-use-git-grep t
 ;; Use ivy for completions.
 projectile-completion-system 'ivy
 ;; When running C-p p to switch projects, prompt for the next projectile
 ;; command instead of running projectile-find-file.
 projectile-switch-project-action 'projectile-commander)

;;; Drop an empty .projectile_root file in a dir for projectile to consider it a
;;; project root
(add-to-list 'projectile-project-root-files ".projectile_root")

;;; Add a easy shortcut for switching to another project's shell.
(def-projectile-commander-method ?x
  "Run shell in project."
  (projectile-run-shell))

;;; Load project shells in predictable windows.
(defun my-projectile-run-shell ()
  "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already
exists. Ensures the shell buffer appears in the current window."
  (interactive)
  (projectile-with-default-dir
      (projectile-ensure-project (projectile-project-root))
    (let* ((bufname (concat "*shell " (projectile-project-name) "*"))
           (buf (get-buffer-create bufname)))
      (pop-to-buffer-same-window buf)
      (shell buf))))

(advice-add 'projectile-run-shell
            :override
            'my-projectile-run-shell)

;;; Enable projectile globally
(projectile-mode)

(provide 'my-projectile)
