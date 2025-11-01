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
  :vc (:url   "https://github.com/xenodium/agent-shell" :rev :latest :branch "main")
  :after (acp shell-maker company)
  :ensure t
  :defer t
  :config (setq agent-shell-google-authentication
                (agent-shell-google-make-authentication :vertex-ai t))

  (setopt agent-shell-file-completion-enabled t))


(use-package eca
  :ensure t)

(provide 'lk/ai-assistance)

;;; ai-assistance.el ends here
