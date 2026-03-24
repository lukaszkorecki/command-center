;; -*- lexical-binding: t; -*-

(use-package load-env-vars
  :vc (:url "https://github.com/lukaszkorecki/emacs-load-env-vars"
            :rev "ccdd7cd9a0abb79c9d3b327b792e192bb87e2eba")
  :ensure t
  :config
  (let ((secrets-init (expand-file-name "~/.emacs.d/private-configs/secrets/secrets-init.el")))
    (if (file-exists-p secrets-init)
        (progn
          (message "Loading secrets from private-configs")
          (load secrets-init))
      (message "private-configs not found, skipping secret loading"))))

(provide 'lk/secrets)
