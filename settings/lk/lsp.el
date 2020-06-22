;; LSP and supporting packages
(use-package yasnippet
  :ensure t)

;; LSP stuffs
(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurescript-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-enable-indentation nil
        lsp-clojure-server-command '("bash" "-c" "clojure-lsp")))

(use-package lsp-ui
  :ensure t
  :bind (("C-c n n" . lsp-ui-find-next-reference)
         ("C-c n p" . lsp-ui-find-prev-reference))
  :commands lsp-ui-mode)

(use-package flycheck
  :ensure t)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-ivy
  :ensure t
  :bind (("C-x c n" . lsp-ivy-workspace-symbol)))




(provide 'lk/lsp)
