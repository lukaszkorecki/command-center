;;; lsp.el -- LSP configuration + supporting packages
;;; Commentary:
;;; Only Clojure is fully supported, other languages might be added

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
        ("C-c l g" . xref-find-definitions)))

(use-package flymake
  :bind (( "C-c e n" . flymake-goto-next-error )
         ( "C-c e p" . flymake-goto-prev-error )
         ( "C-c e l" . flymake-show-buffer-diagnostics)))


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
