;; -*- lexical-binding: t; -*-
;;; /Users/lukasz/.emacs.d/init.el --- /Users/lukasz/.emacs.d/init.el
;;; Commentary:

;;; Code:


;;; Commentary:
;;  loads all customizations and packages

;;; Code:
(load-file "~/.emacs.d/deps.el")

;; load secrets from 1password
(require 'lk/secrets)

;; environment: PATH and exec-path configuration
(require 'lk/env-path)

;; initial startup: environment, GC settings, mise
(require 'lk/startup)

;; display: frames, fonts, colors, window management
(require 'lk/display)

(require 'lk/completion)

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

(require 'lk/apps)

;; HACK
(lk/fix-utf)

(message "Ready")
(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((load-env-vars :url
		    "https://github.com/lukaszkorecki/emacs-load-env-vars"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
