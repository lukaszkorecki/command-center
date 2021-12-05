;;; lsp.el -- LSP configuration + supporting packages
;;; Commentary:
;;; Only Clojure is supported, other languages might be added

;;; Code:
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-clojure-custom-server-command "~/bin/clojure-lsp")
  (setq lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-file-watch-threshold 1000)
  (setq lsp-restart nil)
  (setq lsp-ui-sideline-enable nil)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\node_modules\\'")
  :hook ((clojure-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind
  (("C-c l a" . lsp-ui-sideline--code-actions)))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol
  :bind (("C-c w i" . lsp-ivy-workspace-symbol)))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)


(use-package lsp-ui
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'lk/lsp)
;;; lsp.el ends here
