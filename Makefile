# make make behave properly
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules


# support tooling for mac and linux

kondo_version := 2020.11.07
babashka_version := 0.2.4
cljstyle_version = 0.14.0

os := $(shell uname)
platform := linux
ifeq ($(os),Darwin)
	platform := macos
endif


# tasks

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
	sudo apt-get -y  remove emacs* || true
	sudo add-apt-repository ppa:kelleyk/emacs
	sudo apt-get -y update
	sudo apt install -y emacs26

get-clojure-tools: get-clj-kondo get-bb get-cljstyle


get-clj-kondo:
	curl -L --output /tmp/clj-kondo.zip https://github.com/borkdude/clj-kondo/releases/download/v$(kondo_version)/clj-kondo-$(kondo_version)-$(platform)-amd64.zip
	unzip /tmp/clj-kondo.zip
	mv clj-kondo ~/.emacs.d/etc/bin/



get-bb:
	curl -L --output /tmp/bb.zip https://github.com/borkdude/babashka/releases/download/v$(babashka_version)/babashka-$(babashka_version)-$(platform)-amd64.zip
	unzip /tmp/bb.zip
	mv bb ~/.emacs.d/etc/bin/


get-cljstyle:
	curl -L --output /tmp/cljstyle.tar.gz https://github.com/greglook/cljstyle/releases/download/$(cljstyle_version)/cljstyle_$(cljstyle_version)_$(platform).tar.gz
	tar xzvf /tmp/cljstyle.tar.gz
	mv cljstyle ~/.emacs.d/etc/bin/

.PHONY: all setup packages get-cljstyle get-bb get-clj-kondo  get-clojure-tools
