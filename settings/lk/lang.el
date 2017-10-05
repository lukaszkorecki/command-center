;;; lang.el --- programming language customizations
;;; Commentary:
;;; For now all of these are groupped together but if some modes need more
;;; space, they will be moved to separate files

(setq linum-format "%3d|")

;;; Code:
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

; markdown!
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(setq-default scss-compile-at-save nil)
(setq css-indent-offset 2)

(setq nginx-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.hb$" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.yml$". yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$". yaml-mode))

; Javascripts
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
(setq js-indent-level 2)

;; settings for js2 mode
(add-hook 'js2-mode-hook (lambda () (abbrev-mode)))
(setq-default js-switch-indent-offset 4)
(setq-default js2-basic-offset 2)
(setq-default js2-indent-switch-body t)

;; es6 is ok with trailing commas
(setq-default js2-strict-trailing-comma-warning nil)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

; web-mode stuff
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

;; sh mode
(setq sh-indentation 2)
(setq sh-basic-offset 2)

;; enable emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; better sql indentation
(eval-after-load "sql"
  '(load-library "sql-indent"))

(require 'lk/ruby)
(require 'lk/clojure)
(require 'lk/scheme)
(require 'lk/common-lisp)

(provide 'lk/lang)
