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
	@ln -fvs ~/.emacs.d/etc/clojure-deps.edn ~/.clojure/deps.edn


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
	open -g -a Calendar
	open -g -a MailMate
	open -g -a Linear

stop-comms:
	pkill -f "(Slack|Calendar|MailMate|Linear|Dash)"


start-dev:
	open -g -a Docker
	open -g -a Emacs


stop-dev:
	pkill -f "(Emacs|Docker)"


fix-macos:
# show path bar in finder
	defaults write com.apple.finder ShowPathbar -bool true
# disable .DS_Store files
	defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
	defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Thu 18 Aug 23:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Checked [d MMM]
	sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM HH:mm"
# apply ^^^^
	sudo killall SystemUIServer
