;;; js.el --- Javascript & TS packages, customizations etc
;;; Commentary:

;;; Code:

;; Javascripts, Typescripts and all that crap
(use-package prettier :ensure t)

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


(use-package js-ts-mode
  :ensure t
  :straight js2-mode
  :init
  (add-hook 'js-ts-mode-hook
                  (lambda ()
                    (treesit-font-lock-recompute-features '(property))))
  (add-hook 'js-ts-mode-hook 'eglot-ensure)
  :mode ("\\.js$" . js-ts-mode))

(use-package js2-mode
  :after (js-ts-mode)
  :init (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode)))

(provide 'lk/js)
;;; js.el ends here
