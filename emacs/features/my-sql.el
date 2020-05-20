;;; -*- lexical-binding: t; -*-

;;;; SQL settings
(require 'sql)

;;; Add option to chose port in sql-postgres and use localhost as default
;;; server.
(setq sql-postgres-login-params
      `((user :default ,(user-login-name))
        (database :default ,(user-login-name))
        (server :default "localhost")
        (port :default 5432)))

(add-hook 'sql-mode-hook 'my-whitespace-mode)

(provide 'my-sql)
