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

;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.DotFiles/bin")

(setenv "PATH" (concat (getenv "PATH")
                       ":/usr/local/bin:~/.DotFiles/bin"))

;; reduce GC thrash
(setq gc-cons-threshold 20000000)
(set-language-environment "UTF-8")
(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    (lambda ()
      (add-to-list 'byte-compile-not-obsolete-funcs
                   'preceding-sexp))))


(use-package better-defaults)

(use-package window-number
  :bind
  (("C-c n w" . window-number-switch)))

(require 'lk/helm)

;; make it pretty!
(require 'lk/ui)

;; Editing
;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(use-package whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
;; add final newline automaticaly
(setq require-final-newline t)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(setq x-alt-keysym 'meta) ; make alt work as meta in x11
(define-key global-map (kbd "RET") 'newline-and-indent)

;; no backup files
(setq-default make-backup-files nil)

(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

;; language customizations
(require 'lk/lang)

;; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; load rest of customizations
(require 'lk/customizations)

;; load modeline
(require 'lk/modeline)
