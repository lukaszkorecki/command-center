;;; js.el --- Javascript & TS packages, customizations etc
;;; Commentary:

;;; Code:

;; Javascripts, Typescripts and all that crap
(use-package prettier :ensure t)

(use-package tsx-ts-mode
  :straight (:host github
                   :repo "tree-sitter/tree-sitter-typescript"
                   :path "tsx/src")
  :ensure t
  :after (prettier)
  :init (add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
  (add-hook 'tsx-ts-mode-hook 'prettier-mode)
  :bind (:map tsx-ts-mode-map
              (( "C-x c f" . lk/prettier-format-current-buffer )))
  (:map typescript-mode-map
        (( "C-x c f" . lk/prettier-format-current-buffer ))))

(use-package typescript-ts-mode
  :after (prettier tsx-ts-mode)
  :init (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'prettier-mode)
  (add-hook 'typescript-ts-mode-hook 'prettier-mode)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-to-list 'majo-mode-remap-alist
               '(typescript-mode . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode)))




(provide 'lk/js)
;;; js.el ends here
