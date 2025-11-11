# make make behave properly
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

setup: brew-bundle zsh-completion configs fix-macos


configs:
	@ln -fvs ~/.emacs.d/etc/zshrc ~/.zshrc
	@ln -fvs ~/.emacs.d/etc/zshrc ~/.profile
	@ln -fvs ~/.emacs.d/etc/gitconfig ~/.gitconfig
	@ln -fvs ~/.emacs.d/etc/gitignore_global ~/.gitignore_global
	@mkdir -p ~/.config/clojure-lsp
	@ln -fvs ~/.emacs.d/etc/clojure-lsp-config.edn ~/.config/clojure-lsp/config.edn
	@mkdir -p ~/.clojure/
	@rm -rf ~/.clojure
	@ln -fvs ~/.emacs.d/etc/clojure ~/.clojure
	@ln -fvs ~/.emacs.d/etc/psqlrc ~/.psqlrc
	@ln -fvs ~/.emacs.d/etc/tmux.conf ~/.tmux.conf
	@mkdir -p ~/.lein
	@ln -fvs ~/.emacs.d/etc/lein/profiles.clj ~/.lein/profiles.clj
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
