;; -*- lexical-binding: t; -*-

(use-package load-env-vars :ensure t)

(defun lk/load-secrets-from-1p ()
  (interactive)
  (if (not (getenv "OP_SECRETS_LOADED"))
      (condition-case err
          (progn
            (shell-command-to-string "~/.emacs.d/etc/bin/op-secret-loader -c ~/.private/secrets.edn -f env > /tmp/emacs.env")
            (load-env-vars "/tmp/emacs.env")
            (delete-file "/tmp/emacs.env")
            (message "loaded secrets from 1password"))
        (error (message "Error loading secrets: %s" err)))
    (progn
      (setenv "OP_SECRETS_LOADED" "true")
      (message "already loaded"))))


(when (file-exists-p "~/.private/work-secrets.el")
  (load "~/.private/work-secrets.el"))

(when (fboundp 'lk/load-work-secrets-from-1p)
  (lk/load-work-secrets-from-1p))

(lk/load-secrets-from-1p)

(provide 'lk/secrets)
