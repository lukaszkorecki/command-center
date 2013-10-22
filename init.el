(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)


(package-initialize)

(require 'evil)
(evil-mode 1)

(require 'linum)
(linum-mode 1)
; TODO change this into some lisp magic to
; automatically install packges if not available
; color-theme
; color-theme-solarized-20120301
; evil-1.0.7
; evil-indent-textobject-0.2
; evil-leader-0.4.1
; evil-matchit-0.0.5
; evil-numbers-0.3
; gh-0.7.2
; gist-1.1.1
; gitconfig-1.0.0
; gitignore-mode-0.1
; go-mode-12869
; handlebars-mode-1.3
; hlinum-1.0
; js2-mode-20130619
; logito-0.1
; magit-1.2.0
; makefile-runner-1.1.2
; markdown-mode-1.9
; pcache-0.2.3
; rainbow-mode-0.9
; rspec-mode-1.7
; ruby-mode-1.1
; shell-switcher-0.1.5.1
; undo-tree-0.6.3
