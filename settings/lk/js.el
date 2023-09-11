;;; js.el --- Javascript & TS packages, customizations etc
;;; Commentary:

;;; Code:

;; Javascripts
(defun lk/prettier-format-current-buffer ()
  "."
  (interactive)
  (lk/invoke-compile-tool-in-project "FORCE_COLOR=0 node ./node_modules/.bin/prettier --write %s"))


(use-package tsx-ts-mode
  :straight (:host github
                   :repo "tree-sitter/tree-sitter-typescript"
                   :path "tsx/src")
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
  :bind (:map tsx-ts-mode-map (( "C-x c f" . eglot-format-buffer ))))

(use-package typescript-ts-mode
  :after (tsx-ts-mode)
  :init (setq typescript-indent-level 2)
  (add-to-list 'majo-mode-remap-alist
               '(typescript-mode . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  :bind (:map typescript-ts-mode-map (( "C-x c f" . eglot-format-buffer )))
  (:map tsx-ts-mode-map (( "C-x c f" . eglot-format-buffer)))
  (:map typescript-mode-map (( "C-x c f" . eglot-format-buffer))))



(provide 'lk/js)
;;; js.el ends here
