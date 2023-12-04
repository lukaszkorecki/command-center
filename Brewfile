# -*- mode: ruby -*-
tap "homebrew/bundle"
tap "homebrew/core"
tap 'borkdude/brew'
tap 'd12frosted/emacs-plus'


# core language packages

brew "node"
brew "openjdk"
brew "clojure"
brew "rlwrap"
brew "borkdude/brew/babashka"

# tools
brew "zile"
brew "ripgrep"
brew 'jq'
brew 'borkdude/brew/jet'
brew 'gh'
brew 'tmux'

# infra

brew 'dopplerhq/doppler/doppler'
brew "hashicorp/tap/terraform-ls"

# language specific tooling

brew 'borkdude/brew/clj-kondo'
brew 'leiningen'
brew 'clojure-lsp/brew/clojure-lsp-native'

# Emacs
brew 'libvterm'
brew 'cmake'
brew 'emacs-plus@29', 'with-poll': true, 'with-xwidgets': true, 'with-savchenkovaleriy-big-sur-icon': true

# psql cli, but without PG itself
brew 'libpq', link: true
