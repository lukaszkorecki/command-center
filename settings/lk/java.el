

(use-package lsp-java
  :straight t
  :init
  (setq lsp-java-server-install-dir "~/bin/")
  :config (add-hook 'java-mode-hook 'lsp))
