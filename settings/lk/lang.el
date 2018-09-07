;;; lang.el --- programming language customizations
;;; Commentary:
;;; For now all of these are groupped together but if some modes need more
;;; space, they will be moved to separate files

;;; Code:

(use-package gitignore-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode)))

(use-package python-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

(use-package markdown-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode)))

(use-package jinja2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode)))

(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile.*" . dockerfile-mode)))

(use-package restclient)
(use-package sql-indent)
(use-package terraform-mode)
(use-package ansible)

(use-package scss-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (setq css-indent-offset 2))

(use-package nginx-mode
  :init
  (setq nginx-indent-offset 2))

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$". yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$". yaml-mode)))

;; Javascripts
(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  (setq js-indent-level 2)

  ;; settings for js2 mode
  (add-hook 'js2-mode-hook (lambda () (abbrev-mode)))
  (setq-default js-switch-indent-offset 4)
  (setq-default js2-basic-offset 2)
  (setq-default js2-indent-switch-body t)
  ;; es6 is ok with trailing commas
  (setq-default js2-strict-trailing-comma-warning nil))

(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

;; web-mode stuff
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode)))

;; sh mode
(setq sh-indentation 2)
(setq sh-basic-offset 2)

;; better sql indentation
(eval-after-load "sql"
  '(load-library "sql-indent"))

(require 'lk/ruby)
(require 'lk/clojure)

(provide 'lk/lang)
