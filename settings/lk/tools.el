;; -*- lexical-binding: t; -*-

(use-package gptel
  :straight t
  :init (setq gptel-api-key (getenv "OPENAI_API_KEY")))


(provide 'lk/tools)
