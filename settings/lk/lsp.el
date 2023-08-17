;;; lsp.el -- LSP configuration + supporting packages
;;; Commentary:
;;; Only Clojure is fully supported, other languages might be added

(use-package eglot
  :custom (eglot-confirm-server-initiated-edits nil)
  (eglot-connect-timeout 300)

  :hook ((clojure-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :config (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-autoreconnect t)
  (cl-pushnew
   '((tsx-ts-mode)
     .
     ("typescript-language-server" "--stdio"))
   eglot-server-programs :test #'equal)
  (cl-pushnew
   '((typescript-ts-mode)
     .
     ("typescript-language-server" "--stdio"))
   eglot-server-programs :test #'equal)


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

(defun lk/project-find-root (path)
  "Search up the PATH for known project file markers. Throws an error if found path is
  equal to users home directory"
  (when-let ((root
              (first
               (seq-filter
                (lambda (s) s)
                (mapcar
                 (lambda (f) (locate-dominating-file default-directory f))
                 lk/project-files)))))
    (message (format "Found root %s for path %s" root path))
    (when (string-equal (expand-file-name root) (getenv "HOME"))
      (message "Root folder is equal to HOME!")
      (throw 'lk/invalid-project-root t))
    (cons 'transient (expand-file-name root))))


(add-to-list 'project-find-functions #'lk/project-find-root)

(provide 'lk/lsp)
;;; lsp.el ends here
