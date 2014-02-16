setup: link packages

link:
	ln -s  ~/.emacs.d/init.el  ~/.emacs || true
packages:
	emacs --script ./install-packages.el
	git clone https://github.com/timcharper/evil-surround ~/.emacs.d/elpa/evil-surround
