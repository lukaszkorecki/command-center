;; -*- lexical-binding: t; -*-

(use-package gptel
  :straight t
  :init ;; defaults
  (setq gptel-api-key (lambda () (getenv "OPENAI_API_KEY")))
  (setq gptel-model "gpt-4"))

(use-package mermaid-mode
  :straight t
  :init ; setup
  (setq mermaid-mmdc-location "docker")
  (setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6"))

(defun lk/vterm-project-association ()
  "Associate VTerm buffer with the current project."
  (when-let ((project (project-current)))
    (setq-local project-current project)))

(use-package vterm
  :ensure t
  :init ;
  (setq vterm-shell "/bin/zsh")
  (setq vterm-kill-buffer-on-exit t)
  (add-hook 'vterm-mode-hook 'lk/vterm-project-association)

  :bind (("C-c M-o" . vterm-clear-scrollback)
         ("C-c ESC o" . vterm-clear-scrollback)
         ("C-q" . vterm-send-next-key)))

(use-package multi-vterm
  :ensure t
  :bind (( "C-x t n" . multi-vterm )
         ( "C-x t p" . multi-vterm-project )))

(defun lk/kill-all-vterms ()
  (interactive)
  (lk/kill-buffers-by-major-mode 'vterm-mode))


(use-package keychain-environment
  :init (keychain-refresh-environment))


(provide 'lk/tools)
