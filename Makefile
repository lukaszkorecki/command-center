EMACS_PATH := $(shell which emacs)


setup: link packages

link:
	ln -s  ~/.emacs.d/init.el  ~/.emacs || true


packages:
	EMACS=$(EMACS_PATH) ~/.cask/bin/cask


update:
	EMACS=$(EMACS_PATH) ~/.cask/bin/cask update
