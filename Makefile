
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
	@ln -fvs ~/.emacs.d/etc/cljstyle  ~/.cljstyle

tools:
	@cd ~/.emacs.d/ && emacs -q --batch --no-init-file -l ./deps.el


get-binaries: get-clj-kondo get-bb get-cljstyle


get-clj-kondo: etc/bin/clj-kondo
	curl -L --output /tmp/clj-kondo.zip https://github.com/borkdude/clj-kondo/releases/download/v2020.05.09/clj-kondo-2020.05.09-linux-static-amd64.zip
	unzip /tmp/clj-kondo.zip
	mv clj-kondo ~/.emacs.d/etc/bin/



get-bb: etc/bin/bb
	curl -L --output /tmp/bb.zip https://github.com/borkdude/babashka/releases/download/v0.0.94/babashka-0.0.94-linux-static-amd64.zip
	unzip /tmp/bb.zip
	mv bb ~/.emacs.d/etc/bin/


get-cljstyle: etc/bin/cljstyle
	curl -L --output /tmp/cljstyle.tar.gz https://github.com/greglook/cljstyle/releases/download/0.12.1/cljstyle_0.12.1_linux.tar.gz
	tar xzvf /tmp/cljstyle.tar.gz
	mv cljstyle ~/.emacs.d/etc/bin/
