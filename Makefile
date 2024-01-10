# make make behave properly
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

setup: brew-bundle zsh-completion configs


configs:
	@ln -fvs ~/.emacs.d/etc/zshrc ~/.zshrc
	@ln -fvs ~/.emacs.d/etc/zshrc ~/.profile
	@ln -fvs ~/.emacs.d/etc/gitconfig ~/.gitconfig
	@ln -fvs ~/.emacs.d/etc/gitignore_global ~/.gitignore_global
	@ln -fvs ~/.emacs.d/init.el ~/.emacs
	@mkdir -p ~/.config/clojure-lsp
	@ln -fvs ~/.emacs.d/etc/clojure-lsp-config.edn ~/.config/clojure-lsp/config.edn
	@mkdir -p ~/.clojure/
	@ln -fvs ~/.emacs.d/etc/clojure ~/.clojure
	@ln -fvs ~/.emacs.d/etc/psqlrc ~/.psqlrc
	@ln -fvs ~/.emacs.d/etc/tmux.conf ~/.tmux.conf
	@mkdir -p ~/.lein
	@ln -fvs ~/.emacs.d/etc/lein/profiles.clj ~/.lein/profiles.clj


brew-bundle:
	HOMEBREW_AUTO_UPDATE_SECS=9600 brew bundle

zsh-completion:
	mkdir -p  ~/.emacs.d/etc/zsh/
	ln -fvs /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.zsh ~/.emacs.d/etc/zsh/_git
	ln -fvs /System/Volumes/Data/Applications/Docker.app/Contents/Resources/etc/docker.zsh-completion ~/.emacs.d/etc/zsh/_docker
	ln -fvs  /System/Volumes/Data/Applications/Docker.app/Contents/Resources/etc/docker-compose.zsh-completion ~/.emacs.d/etc/zsh/_docker-compose


# macOS apps things, not related to Emacs

start-comms:
	open -g -a Slack
	open -g -a Fantastical
	open -g -a MailMate
	open -g -a Linear

stop-comms:
	pkill -f "(Slack|MailMate|Linear|Dash)"


start-dev:
	open -g -a Docker
	open -g -a Emacs


stop-dev:
	pkill -f "(Emacs|Docker|Dash)"


fix-macos:
	defaults write com.apple.finder ShowPathbar -bool true
	defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
	defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
	sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM HH:mm"
	# restart to apply
	sudo killall SystemUIServer
