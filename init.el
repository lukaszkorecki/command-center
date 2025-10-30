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



;; initial startup: environment, GC settings, mise
(require 'lk/startup)

;; display: frames, fonts, colors, window management
(require 'lk/display)

;; environment: PATH and exec-path configuration
(require 'lk/env-path)

;; load secrets from 1password
(require 'lk/secrets)

;; workspace: projects, buffers, completion, window navigation
(require 'lk/workspace)

;; AI assistance: copilot, agent-shell, acp
(require 'lk/ai-assistance)

;; git extensions
(require 'lk/git)

;; language server protocol support + related packages
(require 'lk/lsp)

;; terminal: vterm, mermaid, keychain utilities
(require 'lk/terminal)

;; text editing: indents, paren matching, text manipulation
(require 'lk/text-editing)

;; programming language modes: ruby, js, clojure, markdown, etc
(require 'lk/prog-modes)

;; system tweaks: Mac-specific settings, which-key
(require 'lk/system-tweaks)

;; modeline specific settings
(require 'lk/modeline)

;; project dashboard: transient-based project management
(require 'lk/project-dashboard)

;; HACK
(lk/fix-utf)

(message "Ready")
(provide 'init)
