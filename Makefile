# make make behave properly
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

setup: brew-bundle zsh-completion
	@ln -fvs ~/.emacs.d/etc/zshrc ~/.zshrc
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.bashrc
	@ln -fvs ~/.emacs.d/etc/bashrc ~/.profile
	@ln -fvs ~/.emacs.d/etc/gitconfig ~/.gitconfig
	@ln -fvs ~/.emacs.d/etc/gitignore_global ~/.gitignore_global
	@ln -fvs ~/.emacs.d/init.el ~/.emacs
  @mkdir -p ~/.config/clojure-lsp
	@ln -fvs ~/.emacs.d/etc/clojure-lsp-config.edn ~/.config/clojure-lsp/config.edn

brew-bundle:
	HOMEBREW_AUTO_UPDATE_SECS=9600 brew bundle

zsh-completion:
	mkdir -p  ~/.emacs.d/etc/zsh/
	ln -s /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.zsh ~/.emacs.d/etc/zsh/_git
	ln -s /System/Volumes/Data/Applications/Docker.app/Contents/Resources/etc/docker.zsh-completion ~/.emacs.d/etc/zsh/_docker
	ln -s  /System/Volumes/Data/Applications/Docker.app/Contents/Resources/etc/docker-compose.zsh-completion ~/.emacs.d/etc/zsh/_docker-compose


# macOS apps things, not related to Emacs

start-comms:
	open -g -a Slack
	open -g -a Fantastical
	open -g -a MailMate

stop-comms:
	pkill -f "(Slack|Fantastical|MailMate)"


start-dev:
	open -g -a Docker
	open -g -a Emacs


stop-dev:
	pkill -f "(Emacs|Docker)"
