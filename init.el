(load-file "~/.emacs.d/package/init.el")
(load-file "~/.emacs.d/package/evil-init.el")
(evil-leader/set-key
 "|" 'split-window-horizontally
 "-" 'split-window-vertically)


; line numbers
(global-linum-mode 1)
; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

; set colors
(require 'color-theme)
(require 'color-theme-solarized)
(load-theme 'solarized-dark t)
(rainbow-delimiters-mode 1)

; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; indenting
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq indent-line-function 'insert-space)
(define-key global-map (kbd "RET") 'newline-and-indent)
;
; language customizations
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.(md|markdown)$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.hb$" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.(yml|yaml)$". yaml-mode))

; Javascripts
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#7f7f7f")
 '(background-mode dark)
 '(cursor-color "#5c5cff")
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#5c5cff")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

; Rubby
(add-to-list 'auto-mode-alist '("\\.(rb|rake)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^(Vagrantfile|Gemfile|Rakefile|Guardfile)$" . ruby-mode))
