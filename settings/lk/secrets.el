;; -*- lexical-binding: t; -*-

(use-package load-env-vars :ensure t)

(defun lk/load-secrets-from-1p ()
  (interactive)
  (if (not (getenv "OP_SECRETS_LOADED"))
      (condition-case err
          (progn
            (shell-command-to-string "~/.emacs.d/etc/bin/op-secret-loader -c ~/.private/secrets.edn -f env > /tmp/emacs.env")
            (load-env-vars "/tmp/emacs.env")
            (delete-file "/tmp/emacs.env"))
        (error (message "Error loading secrets: %s" err)))

    (message "already loaded")))

(defvar lk/1p-secrets '())

(defun lk/get-secret (account-id secret-path)
  "Get a secret from 1password, but only if it's not already in memory."
  (if (assoc secret-path lk/1p-secrets)
      (cdr (assoc secret-path lk/1p-secrets))
    (let* ((sec
            (shell-command-to-string
             (format "op --account %s read %s" account-id secret-path)))
           (secret (car (split-string sec "\n"))))
      (push (cons secret-path secret) lk/1p-secrets)
      secret)))



(provide 'lk/secrets)
