# make make behave properly
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules


# support tooling for mac and linux

kondo_version := 2021.03.31
babashka_version := 0.3.1
cljstyle_version = 0.15.0
lsp_version = 2021.03.30-20.42.34
node_version = 14.16.0

os := $(shell uname)
platform := linux
ifeq ($(os),Darwin)
	platform := macos
endif


# tasks

all: setup  install-tools packages

setup:
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.bashrc
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.profile
	@ln -fvs ~/.emacs.d/etc/gitconfig ~/.gitconfig
	@ln -fvs ~/.emacs.d/etc/gitignore_global ~/.gitignore_global
	@ln -fvs ~/.emacs.d/etc/lein ~/.lein
	@ln -fvs ~/.emacs.d/init.el ~/.emacs
	@ln -fvs ~/.emacs.d/etc/tmux.conf  ~/.tmux.conf
	@ln -fvs ~/.emacs.d/etc/cljstyle  ~/.cljstyle
	@ln -fvs ~/.emacs.d/etc/wezterm.lua ~/.wezterm.lua

packages:
	@cd ~/.emacs.d/ && /Applications/Emacs.app/Contents/MacOS/Emacs -nw -q --batch --no-init-file -l ./deps.el


ensure-bin:
	mkdir -p ~/bin

install-tools: ensure-bin get-clj-kondo get-bb get-cljstyle get-clojure-lsp install-java install-node install-docker


get-clj-kondo: ensure-bin
	curl -L --output /tmp/clj-kondo.zip https://github.com/borkdude/clj-kondo/releases/download/v$(kondo_version)/clj-kondo-$(kondo_version)-$(platform)-amd64.zip
	unzip /tmp/clj-kondo.zip
	mv clj-kondo ~/bin/



get-bb: ensure-bin
	curl -L --output /tmp/bb.zip https://github.com/borkdude/babashka/releases/download/v$(babashka_version)/babashka-$(babashka_version)-$(platform)-amd64.zip
	unzip /tmp/bb.zip
	mv bb ~/bin/


get-cljstyle: ensure-bin
	curl -L --output /tmp/cljstyle.tar.gz https://github.com/greglook/cljstyle/releases/download/$(cljstyle_version)/cljstyle_$(cljstyle_version)_$(platform).tar.gz
	tar xzvf /tmp/cljstyle.tar.gz
	mv cljstyle ~/bin/


install-java: ensure-bin
	curl -L -v https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.10%2B9/OpenJDK11U-jdk_x64_mac_hotspot_11.0.10_9.tar.gz -O /tmp/openjdk-11_osx-x64_bin.tar.gz
	cd /tmp
	tar xf openjdk-11_osx-x64_bin.tar.gz
	mv openjdk-11_osx-x64_bin ~/bin/jdk


get-clojure-lsp: ensure-bin
	curl -L --output /tmp/clojure-lsp.zip https://github.com/clojure-lsp/clojure-lsp/releases/download/$(lsp_verion)/clojure-lsp-native-macos-amd64.zip
	unzip /tmp/clojure-lsp.zip
	mv clojure-lsp ~/bin/
	chmod +x ~/bin/clojure-lsp


install-node: ensure-bin
	cd /tmp
	curl -L https://nodejs.org/dist/v$(node_version)/node-v$(node_version)-darwin-x64.tar.gz -o  node.tar.gz
	tar zxvf node.tar.gz
	mv node-v$(node_version)-darwin-x64 ~/bin/node
	npm install -g yarn


install-lein: ensure-bin
	curl -L "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein" -o ~/bin/lein
	~/bin/lein


install-docker:
	curl 'https://desktop.docker.com/mac/stable/Docker.dmg' -L -o /tmp/Docker.dmg
	open /tmp/Docker.dmg


.PHONY: all setup packages get-cljstyle get-bb get-clj-kondo  install-tools install-java install-node install-docker ensure-bin
