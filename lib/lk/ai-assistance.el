;;; -*- lexical-binding: t; -*-
;;; ai-assistance.el --- AI-powered coding assistance
;;; Commentary:
;;; Integrates AI tools for code completion and assistance

;;; Code:


(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-emacs-tools-setup))

(provide 'lk/ai-assistance)

;;; ai-assistance.el ends here
