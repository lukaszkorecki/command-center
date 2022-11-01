;;; lsp.el -- LSP configuration + supporting packages
;;; Commentary:
;;; Only Clojure is supported, other languages might be added

;;; Code:
;; (use-package lsp-mode
;;   :init (setq lsp-keymap-prefix "C-c l")
;;   ;; (setq lsp-clojure-custom-server-command "/usr/local/bin/clojure-lsp")
;;   (setq lsp-headerline-breadcrumb-mode nil)
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-keep-workspace-alive nil)
;;   (setq lsp-file-watch-threshold 1000)
;;   (setq lsp-restart nil)
;;   (setq lsp-ui-sideline-enable nil)
;;   :config (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\node_modules\\'")
;;   :hook ((clojure-mode . lsp)
;;          (typescript-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp
;;   :bind (("C-c l a" . lsp-ui-sideline--code-actions)))

;; (use-package lsp-ivy

;;   :commands lsp-ivy-workspace-symbol
;;   :bind (("C-c w i" . lsp-ivy-workspace-symbol)))

;; (use-package lsp-treemacs

;;   :commands lsp-treemacs-errors-list)


;; (use-package lsp-ui)

;; (use-package flycheck

;;   :init (global-flycheck-mode)
;;   :bind (("C-c n e" . flycheck-next-error)))


(use-package eglot
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-connect-timeout 300)
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-autoreconnect t)
  :hook ((clojure-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :bind (("C-c l r r" . eglot-rename)
        ("C-c l f" . eglot-find-declaration)
        ("C-c l a" . eglot-code-actions )
        ("C-c l g" . xref-find-definitions)
        ))

(use-package flymake
  :bind (( "C-c e n" . flymake-goto-next-error )
         ( "C-c e p" . flymake-goto-prev-error )
         ( "C-c e l" . flymake-show-buffer-diagnostics )))


;; FIXME: this overlaps with projectile and it's annoying as fuck.... fix somehow - either migrate of
;; projectile (since I use it very little?) or just leave this here...?
;; see nav.el for projectile-specific configuration
(setq lk/project-files
      '("project.clj" "deps.edn"  "Gemfile"  "package.json" "Makefile" "main.tf" ".git"))

(defun project-find-root (path)
  "Search up the PATH for known project file markers"
  (when-let ((root (first
                    (seq-filter
                     (lambda (s) s)
                     (mapcar
                      (lambda (f)
                        (locate-dominating-file default-directory f))
                      lk/project-files)))))
    (cons 'transient (expand-file-name root))))


(add-to-list 'project-find-functions #'project-find-root)

(provide 'lk/lsp)
;;; lsp.el ends here
