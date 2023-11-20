;; -*- lexical-binding: t; -*-

(use-package gptel
  :straight t
  :init
  ;; defaults
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-mode "gpt4"))


(provide 'lk/tools)
