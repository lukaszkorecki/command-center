setup: link packages

link:
	ln -s  ~/.emacs.d/init.el  ~/.emacs || true
packages:
	git submodule update --init
	/Applications/Emacs.app/Contents/MacOS/Emacs --script ./package/install.el || true
	emacs --script ./package/install.el || true
