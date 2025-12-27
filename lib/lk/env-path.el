;;; -*- lexical-binding: t; -*-
;;; env-path.el --- Environment PATH configuration
;;; Commentary:
;;; Configures PATH and exec-path to ensure custom executables work
;;; in both terminal and GUI Emacs.

;;; Code:

;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path (expand-file-name "~/.local/share/mise/shims"))
(add-to-list 'exec-path (expand-file-name "~/.emacs.d/etc/bin"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))


;; get exec-path items and them into PATH:
;; (setenv "PATH" (concat
;;                 (getenv "PATH")
;;                 (dolist (item exec-path)
;;                   (concat ":" item))))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package keychain-environment
  :ensure t
  :init (keychain-refresh-environment))

(provide 'lk/env-path)
;;; env-path.el ends here
