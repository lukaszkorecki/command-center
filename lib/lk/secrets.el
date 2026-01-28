;; -*- lexical-binding: t; -*-

(use-package load-env-vars
  :vc (:url "https://github.com/lukaszkorecki/emacs-load-env-vars"
            :rev "ccdd7cd9a0abb79c9d3b327b792e192bb87e2eba")
  :ensure t)

(defun lk/load-secrets-from-1p (env-tmpl-path)
  (require 'load-env-vars)
  (load-env-vars-from-string
   (shell-command-to-string
    (format "/opt/homebrew/bin/op --account my.1password.com inject -i %s" (expand-file-name env-tmpl-path)))))

(let ((personal-secrets-loader-el
       (expand-file-name "~/.emacs.d/private-configs/secrets/personal-secrets.el")))
  (when (file-exists-p personal-secrets-loader-el)
    (message "loading custom personal secrets")
    (load personal-secrets-loader-el)))



(let ((work-secrets-loader-el
       (expand-file-name "~/.emacs.d/private-configs/secrets/work-secrets.el")))
  (when (file-exists-p work-secrets-loader-el)
    (message "loading custom work secrets")
    (load work-secrets-loader-el)))

(provide 'lk/secrets)
