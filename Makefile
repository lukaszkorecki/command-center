EMACS_PATH := $(shell which emacs)

PWD := $(shell pwd)

.PHONY: lein

setup: link packages lein

link:
	ln -s  ~/.emacs.d/init.el  ~/.emacs || true


packages:
	EMACS=$(EMACS_PATH) ~/.cask/bin/cask


update:
	EMACS=$(EMACS_PATH) ~/.cask/bin/cask update

lein:
	mkdir -p ~/.lein
	rm -f ~/.lein/profiles.clj
	ln -s $(PWD)/lein/profiles.clj ~/.lein/profiles.clj