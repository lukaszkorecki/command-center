;;; lang.el --- programming language customizations
;;; Commentary:
;;; For now all of these are groupped together but if some modes need more
;;; space, they will be moved to separate files

;;; Code:
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

; markdown!
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode)))

;
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(setq-default scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.hb$" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.yml$". yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$". yaml-mode))

; Javascripts
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (abbrev-mode)))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

; web-mode stuff
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))


(load-file "~/.emacs.d/settings/ruby.el")
