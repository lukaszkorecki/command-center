setup: link packages

link:
	ln -s  ~/.emacs.d/init.el  ~/.emacs || true
packages:
	git submodule update --init
	emacs --script ./package/install.el
