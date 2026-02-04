;;; -*- lexical-binding: t; -*-
;;; ai-assistance.el --- AI-powered coding assistance
;;; Commentary:
;;; Integrates AI tools for code completion and assistance including
;;; GitHub Copilot, agent-shell, and acp. Also includes yasnippet and editorconfig.

;;; Code:

(use-package yasnippet
  :ensure t
  :demand t
  :bind (("C-c i" . yas-insert-snippet)))

(use-package editorconfig :ensure t)

(use-package jsonrpc :ensure t)

(use-package f :ensure t)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
	          :rev :newest
            :branch "main")
  :defer t
  :after (editorconfig jsonrpc f)
  :ensure t
  :config ;
  (setq copilot-max-char 1000000)
  (setq copilot-max-char-warning-disable t)

  ;; NOTE: verify if this is necessary
  (add-to-list 'warning-suppress-types
               '((copilot copilot-exceeds-max-char)))
  (add-to-list 'warning-suppress-types
               '((copilot copilot-no-mode-indent)))
  (add-to-list 'warning-suppress-types
               '((copilot--infer-indentation-offset)))
  (add-to-list 'warning-suppress-types
               '((copilot--infer-indentation-offset)))

  :hook (prog-mode-hook . copilot-mode)
  :bind (("C-x c c" . copilot-accept-completion)))

(use-package shell-maker :ensure t)

(use-package acp
  :vc (:url  "https://github.com/xenodium/acp.el" )
  :after (shell-maker)
  :ensure t)

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell" :rev :latest :branch "main")
  :after (acp shell-maker consult)
  :ensure t
  :defer t
  :config
  ;; Configure the GitHub Copilot command so that we don't get prompts for tool invokation
  ;; I know what I'm doing (mostly)
  (setq agent-shell-github-command
        '("copilot" "--allow-all-tools" "--acp"))

  ;; Enable file completion automatically when starting shells
  (setq agent-shell-file-completion-enabled t))

(use-package eca :ensure t :bind (("C-c d" . eca-transient-menu)))


(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'lk/ai-assistance)

;;; ai-assistance.el ends here
