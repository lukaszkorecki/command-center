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


(provide 'lk/tools)
