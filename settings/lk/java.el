

(use-package lsp-java
  :ensure t
  :init
  (setq lsp-java-server-install-dir "~/bin/")
  :config (add-hook 'java-mode-hook 'lsp))
