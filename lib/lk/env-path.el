;;; env-path.el --- Environment PATH configuration
;;; Commentary:
;;; Configures PATH and exec-path to ensure custom executables work
;;; in both terminal and GUI Emacs.

;;; Code:

;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.emacs.d/etc/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/usr/local/bin:~/.emacs.d/etc/bin:~/bin:~/bin/node/bin:~/bin/jdk/Contents/Home/bin:/usr/local/opt/openjdk/bin:/opt/homebrew/opt/openjdk/bin:/opt/homebrew/bin"))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package keychain-environment
  :ensure t
  :init (keychain-refresh-environment))

(provide 'lk/env-path)
;;; env-path.el ends here
