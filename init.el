;;; /Users/lukasz/.emacs.d/init.el --- /Users/lukasz/.emacs.d/init.el
;;; Commentary:

;;; Code:


;;; Commentary:
;;  loads all customizations and packages

;;; Code:
(load-file "~/.emacs.d/deps.el")


;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.emacs.d/etc/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")

;; saner regex
(require 're-builder)
(setq reb-re-syntax 'string)

;; Ensure we can run remote binaries over ssh
(require 'tramp)
(add-to-list 'tramp-remote-path "~/.emacs.d/etc/bin")
(add-to-list 'tramp-remote-path "/home/ubuntu/.emacs.d/etc/bin")

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; Replicate PATHs from ~/.bashrc, although might not be necessary
;; because of the above
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/usr/local/bin:~/.emacs.d/etc/bin:~/bin:~/bin/node/bin:~/bin/jdk/Contents/Home/bin:/usr/local/opt/openjdk/bin:/opt/homebrew/opt/openjdk/bin:/opt/homebrew/bin"))


;; reduce GC thrash
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 20000000
      read-process-output-max
      (* 1024 1024))

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    (lambda ()
      (add-to-list 'byte-compile-not-obsolete-funcs 'preceding-sexp))))

(use-package better-defaults)

(require 'lk/navigation)

(require 'lk/autocomplete)
(require 'lk/git)
(require 'lk/ui)
(require 'lk/lsp)

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

;; other things

(setq switch-to-buffer-obey-display-actions t) ;; buffer switching? move to UI
(blink-cursor-mode -1) ;; move to UI

(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (alpha-background . 50)))
