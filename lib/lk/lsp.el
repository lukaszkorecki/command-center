;;; -*- lexical-binding: t; -*-
;;; lsp.el -- LSP configuration + supporting packages

(use-package project-rootfile :ensure t)

(defun lk/eglot-ensure-root ()
  "Prevent Eglot from starting if the root directory is $HOME."
  (let ((project-root
         (or (project-root (project-current)) default-directory)))
    (when (string=
           (expand-file-name project-root)
           (expand-file-name "~"))
      (user-error "Eglot won't start in $HOME directory"))))

(use-package flymake
  :ensure t
  :bind (( "C-c e n" . flymake-goto-next-error )
         ( "C-c e p" . flymake-goto-prev-error )
         ( "C-c e l" . flymake-show-buffer-diagnostics)))

(use-package eglot
  :after (project flymake)
  :custom (eglot-confirm-server-initiated-edits nil)
  (eglot-connect-timeout 300)
  ;; Tree-sitter disabled for perf testing — using legacy mode hooks.
  :hook ((clojure-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (js-jsx-mode . eglot-ensure)
         (terraform-mode . eglot-ensure)
         (eglot-managed-mode-hook . lk/eglot-ensure-root)
         (eglot-managed-mode-hook . (lambda () (eglot-inlay-hints-mode 1))))
  :config ;; optimize eglot but keep it usable
  (setq eglot-autoshutdown t)
  (setq eglot-autoreconnect t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-sync-connect 0)
  ;; diable logging, speeds a lot of things up but makes it harder to debug
  (setq eglot-events-buffer-config '(:size 0 :format short))
  ;; I don't use eglot status in mode-line, so this is fine to disable
  (setq eglot-report-progress nil)
  ;; disables polling for code actions, this is unnecessary, because of
  ;; direct binding for eglot-code-actions
  (setq eglot-code-action-indications nil)

  ;; disable LSP features that I don't use
  (add-to-list 'eglot-ignored-server-capabilities :colorProvider)
  (add-to-list 'eglot-ignored-server-capabilities :foldingRangeProvider)

  (setq eglot-autoshutdown t)
  (add-to-list 'project-find-functions #'project-rootfile-try-detect)

  (require 'transient)
  (transient-define-prefix lk/lsp
    ()
    "LSP actions"
    [["Navigate"
      ("g" "Definition"          xref-find-definitions)
      ("d" "Definition (window)" xref-find-definitions-other-window)
      ("u" "References"          xref-find-references)
      ("s" "Symbol in project"   xref-find-apropos)]
     ["Edit"
      ("r" "Rename"       eglot-rename)
      ("a" "Code actions" eglot-code-actions)
      ("f" "Format"       eglot-format)]
     ["Diagnostics"
      ("n" "Next error"  flymake-goto-next-error :transient t)
      ("p" "Prev error"  flymake-goto-prev-error :transient t)
      ("l" "List errors" flymake-show-buffer-diagnostics)]
     ["Server"
      ("R" "Reconnect" eglot-reconnect)
      ("S" "Shutdown"  eglot-shutdown)
      ("E" "Events"    eglot-events-buffer)]])

  (global-set-key (kbd "C-c l") 'lk/lsp))

(provide 'lk/lsp)
;;; lsp.el ends here
