all: setup packages install-emacs get-clojure-tools

install-emacs:
	sudo apt-get remove emacs* || true
	sudo add-apt-repository ppa:kelleyk/emacs
	sudo apt-get -y update
	sudo apt install -y emacs26

setup:
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.bashrc
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.profile
	@ln -fvs ~/.emacs.d/etc/gitconfig ~/.gitconfig
	@ln -fvs ~/.emacs.d/etc/gitignore_global ~/.gitignore_global
	@ln -fvs ~/.emacs.d/etc/lein ~/.lein
	@ln -fvs ~/.emacs.d/init.el ~/.emacs
	@ln -fvs ~/.emacs.d/etc/tmux.conf  ~/.tmux.conf
	@ln -fvs ~/.emacs.d/etc/cljstyle  ~/.cljstyle

packages:
	@cd ~/.emacs.d/ && emacs -q --batch --no-init-file -l ./deps.el

install-emacs:
	sudo apt-get remove emacs* || true
	sudo add-apt-repository ppa:kelleyk/emacs
	sudo apt-get -y update
	sudo apt install -y emacs26

get-clojure-tools: get-clj-kondo get-bb get-cljstyle get-clojure-lps


get-clj-kondo:
	curl -L --output /tmp/clj-kondo.zip https://github.com/borkdude/clj-kondo/releases/download/v2020.05.09/clj-kondo-2020.05.09-linux-static-amd64.zip
	unzip /tmp/clj-kondo.zip
	mv clj-kondo ~/.emacs.d/etc/bin/



get-bb:
	curl -L --output /tmp/bb.zip https://github.com/borkdude/babashka/releases/download/v0.0.94/babashka-0.0.94-linux-static-amd64.zip
	unzip /tmp/bb.zip
	mv bb ~/.emacs.d/etc/bin/


get-cljstyle:
	curl -L --output /tmp/cljstyle.tar.gz https://github.com/greglook/cljstyle/releases/download/0.12.1/cljstyle_0.12.1_linux.tar.gz
	tar xzvf /tmp/cljstyle.tar.gz
	mv cljstyle ~/.emacs.d/etc/bin/

get-clojure-lsp:
	curl -L --output /tmp/clojure-lsp https://github.com/snoe/clojure-lsp/releases/download/release-20200514T134144/clojure-lsp
	mv /tmp/clojure-lsp ~/.emacs.d/etc/bin/
	chmod +x  ~/.emacs.d/etc/bin/clojure-lsp

.PHONY: all setup packages get-binaries get-cljstyle get-bb get-clj-kondo get-clojure-lsp get-clojure-tools
