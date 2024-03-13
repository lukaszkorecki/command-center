;; -*- lexical-binding: t; -*-

(use-package gptel
  :straight t
  :init ;; defaults
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-model "gpt-4"))


(use-package mermaid-mode
  :straight t
  :init ; setup
  (setq mermaid-mmdc-location "docker")
  (setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6"))



(use-package vterm
  :ensure t
  :init
  (setq vterm-shell "/bin/zsh")
  (setq vterm-kill-buffer-on-exit t)
  :bind (("C-c M-o" . vterm-clear-scrollback)
         ("C-c ESC o" . vterm-clear-scrollback)))

(use-package multi-vterm
  :ensure t
  :bind (( "C-x t n" . multi-vterm )
         ( "C-x t p" . multi-vterm-project )))

(provide 'lk/tools)
