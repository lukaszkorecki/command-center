(load-file "~/.emacs.d/package.el")
; initialize evil
(require 'evil)
(evil-mode 1)
(global-linum-mode 1)

; set colors
(require 'color-theme)
(require 'color-theme-solarized)
(load-theme 'solarized-dark t)

; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; indenting
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq indent-line-function 'insert-space)
(define-key global-map (kbd "RET") 'newline-and-indent)

; language customizations
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.gitgnore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.md|markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.hb\\'" . handlebars-mode))
