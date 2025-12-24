;;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :mode ( "\\.liquid$"  "\\.html$" "\\.hb$" "\\.mustache$" "\\.erb$" )
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-engines-alist
                '(("jinja"    . "\\.j2\\'"))))

;; (use-package css-ts-mode
;;   :ensure t
;;   :mode ("\\.css\\'" "\\.scss\\'" "\\.less\\'")
;;   :defer 't

;;   :config (add-to-list 'treesit-language-source-alist
;;                        '(css .
;;                              ("https://github.com/tree-sitter/tree-sitter-css" "0.25.0" "src"))))

(use-package typescript-ts-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'" )
  :defer 't

  :preface (dolist
               (mapping '((typescript-mode . typescript-ts-mode)))
             (add-to-list 'major-mode-remap-alist mapping))
  (dolist (mapping '((js-mode . js-jsx-mode) (js2-mode . js-jsx-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config

  (add-to-list 'treesit-language-source-alist
               '(tsx .
                     ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")))
  (add-to-list 'treesit-language-source-alist
               '(typescript .
                            ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")))

  (add-to-list 'treesit-language-source-alist
               '(javascript .
                            ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src")))

  ;; activate js-jsx-mode when opening .js or .jsx files - rather than ts-mode since that's usually not what we want
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
  )

(provide 'lk/frontend)
