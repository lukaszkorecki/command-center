;;; /Users/lukasz/.emacs.d/init.el --- /Users/lukasz/.emacs.d/init.el
;;; Commentary:

;;; Code:


;;; Commentary:
;;  loads all customizations and packages

;;; Code:
(load-file "~/.emacs.d/deps.el")


;; reduce GC thrash
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 20000000
      read-process-output-max
      (* 1024 1024))

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    (lambda ()
      (add-to-list 'byte-compile-not-obsolete-funcs 'preceding-sexp))))

;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.emacs.d/etc/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/usr/local/bin:~/.emacs.d/etc/bin:~/bin:~/bin/node/bin:~/bin/jdk/Contents/Home/bin:/usr/local/opt/openjdk/bin:/opt/homebrew/opt/openjdk/bin:/opt/homebrew/bin"))

(setq warning-minimum-level :error)
(setq ring-bell-function 'ignore)

(setenv "INSIDE_EMACS" "TRUE")
(use-package better-defaults)

;; saner regex
(require 're-builder)
(setq reb-re-syntax 'string)
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))


;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq abbrev-file-name "~/.emacs.d/abbrev.el")



(require 'lk/secrets)
(require 'lk/navigation)
(require 'lk/ui)
(require 'lk/autocomplete)
(require 'lk/git)
(require 'lk/lsp)
(require 'lk/tools)
(require 'lk/editing)
(require 'lk/lang)
(require 'lk/customizations)
(require 'lk/modeline)
(require 'lk/proj-mgr)



;; other things
;; Always start server, useful for things
(load "server")
(unless (server-running-p)
  (server-start))

(lk/load-secrets-from-1p)
(lk/fix-utf)
