;; -*- lexical-binding: t; -*-

(use-package load-env-vars
  :vc (:url "https://github.com/lukaszkorecki/emacs-load-env-vars"
            :rev "ccdd7cd9a0abb79c9d3b327b792e192bb87e2eba")
  :ensure t)

(defun lk/load-secrets-from-1p (env-tmpl-path)
  (require 'load-env-vars)
  (load-env-vars-from-string
   (shell-command-to-string
    (format "/opt/homebrew/bin/op inject -i %s"
            (expand-file-name env-tmpl-path))))

  (setenv "OP_SECRETS_LOADED" "1")
  )

(lk/load-secrets-from-1p "~/.private/secrets.env.tmpl")

(defun lk/force-load-secrets ()
  (interactive)
  (lk/load-secrets-from-1p "~/.private/secrets.env.tmpl"))

(when (file-exists-p "~/.private/work-secrets.el")
  (message "loading custom work secrets")
  (load "~/.private/work-secrets.el"))

(provide 'lk/secrets)
