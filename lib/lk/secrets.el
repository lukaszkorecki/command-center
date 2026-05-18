;; -*- lexical-binding: t; -*-

;; load-env-vars is vendored under ~/.emacs.d/vendor to keep MELPA's older
;; 0.0.2 release from clobbering the fork that ships `load-env-vars-from-string'.
(require 'load-env-vars)

(let ((secrets-init (expand-file-name "~/.emacs.d/private-configs/secrets/secrets-init.el")))
  (if (file-exists-p secrets-init)
      (progn
        (message "Loading secrets from private-configs")
        (load secrets-init))
    (message "private-configs not found, skipping secret loading")))

(provide 'lk/secrets)
