;;; -*- lexical-binding: t; -*-
;;; ai-assistance.el --- AI-powered coding assistance
;;; Commentary:
;;; Integrates AI tools for code completion and assistance including
;;; GitHub Copilot, agent-shell, and acp. Also includes yasnippet and editorconfig.

;;; Code:


(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config (claude-code-ide-emacs-tools-setup))

(provide 'lk/ai-assistance)

;;; ai-assistance.el ends here
