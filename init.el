;;;; init --- loads all customizations and packages
;;;; Commentary - Prolly needs splitting more
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/settings")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.emacs.d/etc/bin")

(setenv "PATH" (concat (getenv "PATH")
                       ":/usr/local/bin:~/.emacs.d/etc/bin"))

;; reduce GC thrash
(setq gc-cons-threshold 20000000)
(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    (lambda ()
      (add-to-list 'byte-compile-not-obsolete-funcs
                   'preceding-sexp))))

(use-package better-defaults
  :ensure t)

(use-package window-number
  :ensure t
  :init
  (global-unset-key (kbd "C-j"))
  :bind
  (("C-j" . window-number-switch)))

(require 'lk/ivy)
(require 'lk/company)
(require 'lk/ui)

;; Editing
(require 'lk/editing)

;; no backup files
(setq-default make-backup-files nil)
;; no lockfiles
(setq create-lockfiles nil)

(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

;; language customizations
(require 'lk/lang)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

;; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq abbrev-file-name "~/.emacs.d/abbrev.el")


;; load rest of customizations
(require 'lk/customizations)

;; load modeline
(require 'lk/modeline)
