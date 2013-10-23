(load-file "package.el")
(package-list-packages)
(defvar lk-packages
  '(color-theme-solarized color-theme
		evil evil-indent-textobject evil-leader evil-matchit evil-numbers
    gitignore-mode
		go-mode
		handlebars-mode
		markdown-mode
		rspec-mode ruby-mode
		js2-mode
		magit
    makefile-runner  undo-tree))

(loop for p in lk-packages
      when (not (package-installed-p p)) do (package-install p))
