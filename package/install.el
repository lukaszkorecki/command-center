;;; package/install --- install packages listed in evil.el and my.el
;; Commentary:
;; Poor man's cask emulator.
;;; kinda.

;;; Code:
(load-file "~/.emacs.d/package/init.el")
(package-list-packages)

(defvar lk-evil-packages '(evil
                           evil-indent-textobject
                           evil-leader
                           evil-matchit
                           evil-numbers))

(defvar lk-my-packages '(rubocop
                         color-theme-solarized
                         color-theme-sanityinc-tomorrow
                         color-theme
                         coffee-mode
                         lua-mode
                         gitignore-mode
                         web-mode
                         go-mode
                         handlebars-mode
                         markdown-mode
                         rspec-mode
                         ruby-mode
                         js2-mode
                         yaml-mode
                         json-mode
                         python-mode
                         magit
                         makefile-runner
                         rainbow-delimiters
                         scss-mode
                         projectile
                         grizzl
                         flycheck
                         move-text
                         undo-tree
                         puppet-mode))

(defun lk-install-from-list (list)
  "Install all packages from given LIST, only if not installed."
  (mapc (lambda (name)
	  (message "Installing %s" name)
	  (if (package-installed-p name)
	      (message "  Package %s is already installed" name)
        (package-install name))) list))


(lk-install-from-list lk-my-packages)
(lk-install-from-list lk-evil-packages)

(provide 'install)
;;; install.el ends here
