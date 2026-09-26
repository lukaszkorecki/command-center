;; -*- lexical-binding: t; -*-
;;; /Users/lukasz/.emacs.d/init.el --- /Users/lukasz/.emacs.d/init.el
;;; Commentary:

;;; Code:

;;; Commentary:
;;  loads all customizations and packages

;;; Code:
(load-file "~/.emacs.d/deps.el")

;; environment: PATH and exec-path configuration
(require 'lk/env-path)

;; load secrets from 1password
(require 'lk/secrets)

;; initial startup: environment, GC settings, mise
(require 'lk/startup)

;; system tweaks: Mac-specific settings, which-key
(require 'lk/system-tweaks)

;; display: frames, fonts, colors, window management
(require 'lk/display)

(require 'lk/completion)

;; git extensions
(require 'lk/git)

;; language server protocol support + related packages
(require 'lk/lsp)

;; terminal: ghostel, mermaid, keychain utilities
(require 'lk/terminal)

;; text editing: indents, paren matching, text manipulation
(require 'lk/text-editing)

;; programming language modes: ruby, js, clojure, markdown, etc
(require 'lk/prog-modes)

;; modeline specific settings
(require 'lk/modeline)

;; AI assistance: agent-shell, acp
(require 'lk/ai-assistance)

(require 'lk/apps)

(require 'lk/orgmode)
;; HACK
(lk/fix-utf)

(message "Ready")
(provide 'init)
