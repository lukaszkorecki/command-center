;;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :mode ( "\\.liquid$"  "\\.html$" "\\.hb$" "\\.mustache$" "\\.erb$" )
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-engines-alist
                '(("jinja"    . "\\.j2\\'"))))

;; Tree-sitter disabled for perf testing — using legacy typescript-mode and js-jsx-mode.
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :defer 't

  :preface
  (dolist (mapping '((js-mode . js-jsx-mode) (js2-mode . js-jsx-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (setq js-indent-level 2)
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(provide 'lk/frontend)
