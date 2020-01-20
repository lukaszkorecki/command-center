
.PHONY: all

all: setup tools

setup:
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.bashrc
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.profile
	@ln -fvs ~/.emacs.d/etc/gitconfig ~/.gitconfig
	@ln -fvs ~/.emacs.d/etc/gitignore_global ~/.gitignore_global
	@ln -fvs ~/.emacs.d/etc/lein ~/.lein
	@ln -fvs ~/.emacs.d/init.el ~/.emacs
	@ln -fvs ~/.emacs.d/etc/tmux.conf  ~/.tmux.conf

tools:
	@cd ~/.emacs.d/ && emacs -q --batch --eval "(progn (require 'package) (add-to-list 'package-archives '(\"gnu\" . \"http://elpa.gnu.org/packages/\") t) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (setq package-check-signature nil) (package-initialize) (package-refresh-contents) (package-install 'use-package))"
