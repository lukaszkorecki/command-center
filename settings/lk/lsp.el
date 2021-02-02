(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-clojure-custom-server-command "~/bin/clojure-lsp")
  (setq lsp-headerline-breadcrumb-mode nil)
  :hook ((clojure-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)


(use-package lsp-ui
  :ensure t)

(provide 'lk/lsp)
