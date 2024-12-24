;;; js.el --- Javascript & TS packages, customizations etc
;;; Commentary:

;;; Code:

;; Javascripts, Typescripts and all that crap
(use-package typescript-ts-mode
  :straight (:host github
                   :repo "tree-sitter/tree-sitter-typescript"
                   :path "typescript/src")
  :after (prettier)
  :init (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))
  :config (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'prettier-mode)
  (add-hook 'typescript-ts-mode-hook 'prettier-mode)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-to-list 'major-mode-remap-alist
               '(typescript-mode . typescript-ts-mode)))

(use-package tsx-ts-mode
  :straight (:host github
                   :repo "tree-sitter/tree-sitter-typescript"
                   :path "tsx/src")
  :ensure t
  :after (prettier typescript-ts-mode)
  :init (add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
  (add-hook 'tsx-ts-mode-hook 'prettier-mode)
  :bind (:map tsx-ts-mode-map
              (( "C-x c f" . lk/prettier-format-current-buffer )))
  (:map typescript-mode-map
        (( "C-x c f" . lk/prettier-format-current-buffer ))))

;; We need this so that restclient doesn't blow up
(defun js-mode () )

(use-package js2-mode
  :ensure t
  :init (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs
                (list "window" "module" "require" "sinon"  "setTimeout" "clearTimeout" "setInterval" "clearInterval"  "console" "JSON"))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  :bind (("C-c C-t" . copilot-accept-completion)))

(provide 'lk/js)
;;; js.el ends here
