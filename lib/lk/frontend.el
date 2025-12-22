(use-package web-mode
  :ensure t
  :mode ( "\\.liquid$"  "\\.html$" "\\.hb$" "\\.mustache$" "\\.erb$" )
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-engines-alist
                '(("jinja"    . "\\.j2\\'"))))

(use-package typescript-ts-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'" "\\.jsx\\'" "\\.mjs\\'" "\\.cjs\\'")
  :defer 't

  :preface (dolist
               (mapping
                '((typescript-mode . typescript-ts-mode)
                  (js-mode . typescript-ts-mode)
                  (js2-mode . typescript-ts-mode)

                  (css-mode . css-ts-mode)
                  (json-mode . json-ts-mode)))
             (add-to-list 'major-mode-remap-alist mapping))

  :config
  (add-to-list 'treesit-language-source-alist
               '(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src")))
  (add-to-list 'treesit-language-source-alist
               '(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")))
  (add-to-list 'treesit-language-source-alist
               '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))))

(provide 'lk/frontend)
