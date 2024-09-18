# -*- mode: ruby -*-
tap "homebrew/bundle"
tap 'borkdude/brew'
tap 'd12frosted/emacs-plus'
tap 'hashicorp/tap'
tap 'dopplerhq/doppler'
tap 'clojure-lsp/brew'

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
cask 'todoist'
brew 'bat'

tap 'aviator-co/tap'
brew 'av'
# infra & secrets

brew 'dopplerhq/doppler/doppler'
brew "hashicorp/tap/terraform-ls"
cask '1password-cli'

# language specific tooling

brew 'borkdude/brew/clj-kondo'
brew 'leiningen'
brew 'clojure-lsp/brew/clojure-lsp-native'

# Emacs
brew 'libvterm'
brew 'cmake'
brew 'emacs-plus@30',
     'with-poll': true,
     'with-xwidgets': true,
     'with-modern-icon': true,
     link: true

# psql cli, but without PG itself
brew 'libpq', link: true
brew 'shfmt'
