(load-file "~/.emacs.d/package.el")
(require 'evil)
(evil-mode 1)
(global-linum-mode 1)

(require 'color-theme)
(require 'color-theme-solarized)

(load-theme 'solarized-dark t)

