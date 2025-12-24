;; -*- lexical-binding: t; -*-

(use-package load-env-vars :ensure t)

(defun lk/load-secrets-from-1p (control-var env-tmpl-path)
  (interactive)
  (require 'load-env-vars)
  (if (file-exists-p env-tmpl-path)
      (progn
        (if (not (getenv control-var))
            (condition-case err
                (let* ((tmp-env-file-path
                        (make-temp-file "emacs-secrets" nil ".env")))
                  (shell-command-to-string
                   (format "op inject -i  -o %s"
                           (expand-file-name env-tmpl-path tmp-env-file-path)))
                  (load-env-vars tmp-env-file-path)
                  (delete-file tmp-env-file-path)
                  (setenv control-var "true")
                  (message "loaded secrets from 1password via %s" env-tmpl-path))
              (error (message "Error loading secrets: %s" err)))
          (progn
            (setenv "OP_SECRETS_LOADED" "true")
            (message "already loaded"))))

    (message "ERROR: env template file doesn't exist: %s" env-tmpl-path)))

(lk/load-secrets-from-1p "OP_SECRETS_LOADED" "~/.private/secrets.env.tmpl")

(when (file-exists-p "~/.private/work-secrets.el")
  (message "loading custom work secrets")
  (load "~/.private/work-secrets.el"))

(provide 'lk/secrets)
