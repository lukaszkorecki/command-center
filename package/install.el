;;; package/install --- install packages
;; Poor man's cask emulator.
;;; kinda.

;;; Code:
(load-file "~/.emacs.d/package/init.el")
(package-list-packages)
(defvar lk/my-packages '(rubocop
                         color-theme-solarized
                         color-theme-sanityinc-tomorrow
                         color-theme
                         coffee-mode
                         lua-mode
                         clojure-mode
                         clojure-env
                         slime-clj
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

(defun lk/install-from-list (list)
  "Install all packages from given LIST, only if not installed."
  (mapc (lambda (name)
	  (message "Installing %s" name)
	  (if (package-installed-p name)
	      (message "  Package %s is already installed" name)
        (package-install name))) list))


(lk/install-from-list lk/my-packages)

(provide 'install)
;;; install.el ends here
