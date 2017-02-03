;;; -*- lexical-binding: t; -*-

;;; Tramp customizations

(eval-when-compile
  (require 'tramp))

(custom-set-variables
 ;;; Enable sudo on remote servers
 '(tramp-default-proxies-alist
   (let ((local-host-regex (concat "\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\|" system-name "\\)")))
     `((,local-host-regex "root" nil) (".*" "root" "/ssh:%h:"))))
 ;; Disable version control checks on remote files
 '(vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
 ;; Use ssh control master config from ~/.ssh/config
 '(tramp-ssh-controlmaster-options ""))

(with-eval-after-load 'tramp
  ;; Always add $HOME/bin to the remote path
  (add-to-list 'tramp-remote-path "~/bin"))

(provide 'my-tramp)
