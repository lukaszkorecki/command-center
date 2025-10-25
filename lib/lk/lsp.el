;;; lsp.el -- LSP configuration + supporting packages

(defun lk/eglot-ensure-root ()
  "Prevent Eglot from starting if the root directory is $HOME."
  (let ((project-root
         (or (project-root (project-current)) default-directory)))
    (when (string= project-root (expand-file-name "~"))
      (user-error "Eglot won't start in $HOME directory"))))

(use-package flymake
  :ensure t
  :bind (( "C-c e n" . flymake-goto-next-error )
         ( "C-c e p" . flymake-goto-prev-error )
         ( "C-c e l" . flymake-show-buffer-diagnostics)))

(use-package eglot
  :after (project flymake)
  :custom ;
  (eglot-confirm-server-initiated-edits nil)
  (eglot-connect-timeout 300)
  :hook ((clojure-ts-mode . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (rjsx-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (eglot-managed-mode-hook . (lambda () (flymake-mode t)))
         (eglot-managed-mode-hook . lk/eglot-ensure-root))
  :config ;
  (setq eglot-autoshutdown t)
  (eglot-inlay-hints-mode 1)
  (setq eglot-autoreconnect t)
  (setq eglot-confirm-server-initiated-edits nil)

  (cl-pushnew
   '((tsx-ts-mode) . ("typescript-language-server" "--stdio"))
   eglot-server-programs :test #'equal)

  (cl-pushnew
   '((rjsx-mode) . ("typescript-language-server" "--stdio"))
   eglot-server-programs :test #'equal)

  (cl-pushnew
   '((typescript-ts-mode) . ("typescript-language-server" "--stdio"))
   eglot-server-programs :test #'equal)

  (cl-pushnew
   '((terraform-mode) . ("terraform-ls" "serve"))
   eglot-server-programs :test #'equal)

  (cl-pushnew
   '((sh-mode) . ("bash-language-server" "start"))
   eglot-server-programs :test #'equal)

  :bind (("C-c l r r" . eglot-rename)
         ("C-c l a" . eglot-code-actions )
         ("C-c l g" . xref-find-definitions)
         ("C-c l d" . xref-find-definitions-other-window)
         ("C-c l u" . xref-find-references)
         ("C-x c f" . eglot-format )))

(use-package xref
  :after (consult eglot)
  :ensure t
  :config (setq xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref))

(provide 'lk/lsp)
;;; lsp.el ends here
