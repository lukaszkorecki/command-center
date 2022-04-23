;;; java.el --- Java support stuff
;;; Commentary:

;;; Code:



(use-package lsp-java
  :init (setq lsp-java-server-install-dir "~/bin/")
  :config (add-hook 'java-mode-hook 'lsp))


(provide 'lk/java)
;;; java.el ends here
