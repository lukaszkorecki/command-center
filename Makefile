# make make behave properly
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

setup: brew-bundle zsh-completion configs fix-macos

clojure-configs:
	@mkdir -p ~/.config/clojure-lsp
	@ln -fvs ~/.emacs.d/etc/clojure-lsp-config.edn ~/.config/clojure-lsp/config.edn
	@mkdir -p ~/.clojure/
	@rm -rf ~/.clojure
	@ln -fvs ~/.emacs.d/etc/clojure ~/.clojure
	@rm -rf ~/.lein
	@ln -fvs ~/.emacs.d/etc/lein ~/.lein

# FIXME: this is silly...
ghostty-configs:
	@mkdir -p ~/.config/ghostty
	@echo "config-file = $(HOME)/.emacs.d/etc/ghostty/config" > ~/.config/ghostty/config
	@echo "theme = $(HOME)/.emacs.d/etc/ghostty/themes/modus-operandi" >> ~/.config/ghostty/config
	@mv ~/.config/ghostty/config ~/Library/Application\ Support/com.mitchellh.ghostty/config

private-configs:
	git submodule update --init --recursive private-configs
	cd private-configs && git switch main && git update && make setup

configs: clojure-configs ghostty-configs private-configs
	@ln -fvs ~/.emacs.d/etc/zshrc ~/.zshrc
	@ln -fvs ~/.emacs.d/etc/zshrc ~/.profile
	@ln -fvs ~/.emacs.d/etc/gitconfig ~/.gitconfig
	@ln -fvs ~/.emacs.d/etc/psqlrc ~/.psqlrc
	@mkdir -p ~/.config/mise
	@ln -fvs ~/.emacs.d/etc/mise.toml ~/.config/mise/config.toml

brew-bundle:
	HOMEBREW_AUTO_UPDATE_SECS=9600 brew bundle

zsh-completion:
	mkdir -p  ~/.emacs.d/etc/zsh/
	ln -fvs /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.zsh ~/.emacs.d/etc/zsh/_git
	ln -fvs /System/Volumes/Data/Applications/Docker.app/Contents/Resources/etc/docker.zsh-completion ~/.emacs.d/etc/zsh/_docker
	ln -fvs  /System/Volumes/Data/Applications/Docker.app/Contents/Resources/etc/docker-compose.zsh-completion ~/.emacs.d/etc/zsh/_docker-compose

fix-macos:
	defaults write com.apple.finder ShowPathbar -bool true
	defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
	defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
	sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM HH:mm"
	sudo killall SystemUIServer


# instals things that need special handling
install-other-tools: install-emacs
	bbin install https://github.com/bhauman/clojure-mcp-light.git --tag v0.2.1 --as clj-nrepl-eval --main-opts '["-m" "clojure-mcp-light.nrepl-eval"]'
	bbin install https://github.com/bhauman/clojure-mcp-light.git --tag v0.2.1 --as clj-paren-repair --main-opts '["-m" "clojure-mcp-light.paren-repair"]'


install-emacs:
	curl 'https://emacsformacosx.com/emacs-builds/Emacs-2025-12-21_00-09-31-1eb247af73c3dbfbf8d4c4363d1a22e3fbcf6ce7-universal.dmg' -L -o /tmp/emacs.dmg && \
	  hdiutil attach /tmp/emacs.dmg && \
	  cp -R /Volumes/Emacs/Emacs.app /Applications/Emacs.app && \
	  hdiutil detach /Volumes/Emacs && \
	  rm /tmp/emacs.dmg


.PHONY: setup brew-bundle zsh-completion fix-macos configs clojure-configs ghostty-configs private-configs install-other-tools
