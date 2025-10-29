;;; /Users/lukasz/.emacs.d/init.el --- /Users/lukasz/.emacs.d/init.el
;;; Commentary:

;;; Code:


;;; Commentary:
;;  loads all customizations and packages

;;; Code:
(load-file "~/.emacs.d/deps.el")

;; customizations file
(when (file-exists-p "~/.emacs.d/custom.el")
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

(setq abbrev-file-name "~/.emacs.d/abbrev.el")
;; make sure transient is available everyhwere
(require 'transient)

;; initial startup
(require 'lk/boot)

;; FIXME: rename all of these to something sensible - they don't make sense anymore

;; sets up modeline, tabs and other things
(require 'lk/ui)

;; ensures that PATH is set and sync'd between Emacs and everything else
(require 'lk/setup-path)

;; load secrets from 1password
(require 'lk/secrets)

;; project.el window management, completions
(require 'lk/navigation)

;; AI tools mostly,not only company
(require 'lk/autocomplete)

;; git extensions
(require 'lk/git)

;; language server protocol support + related packages
(require 'lk/lsp)

;; things thar are not for programming
(require 'lk/tools)

;; general editing settings: indents, paren matching etc
(require 'lk/editing)

;; various major modes for programming etc, also sets up ruby, js, clojure, markdown in sepearate packages
(require 'lk/lang)

;; emacs customizations extending its basic functionality
(require 'lk/customizations)

;; modeline specific settings
(require 'lk/modeline)

;; FIXME: integrate this with
(require 'lk/proj-mgr)

;; HACK
(lk/fix-utf)
(provide 'init)
