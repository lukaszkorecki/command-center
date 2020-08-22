;;;; init --- loads all customizations and packages
(package-initialize)

(load-file "~/.emacs.d/deps.el")

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

(require 'lk/navigation)
(require 'lk/autocomplete)
(require 'lk/git)
(require 'lk/ui)

;; Editing
(require 'lk/editing)


;; language customizations
(require 'lk/lang)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

;; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq abbrev-file-name "~/.emacs.d/abbrev.el")

;; load rest of customizations and extra packages
(require 'lk/customizations)

;; perhaps org, erc, etc
(require 'lk/apps)

;; load modeline
(require 'lk/modeline)
(put 'narrow-to-region 'disabled nil)

(defun lk/fix-utf ()
  (interactive)
  ;; unicode rules everything around me
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (define-coding-system-alias 'UTF-8 'utf-8))

(lk/fix-utf)
