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

(projectile-mode)

(provide 'my-projectile)
