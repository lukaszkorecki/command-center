setup: link packages

link:
	ln -s  ~/.emacs.d/init.el  ~/.emacs || true


packages:
	~/.cask/bin/cask


update:
	~/.cask/bin/cask update
