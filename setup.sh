#!/usr/bin/env bash
set -euo pipefail

EMACS_D="${HOME}/.emacs.d"
ETC="${EMACS_D}/etc"
PRIVATE="${EMACS_D}/private-configs"
GET_PROFILE="${PRIVATE}/lib/get-profile.sh"
ZSH_COMP_DIR="${ETC}/zsh"
GHOSTTY_XDG="${HOME}/.config/ghostty"
GHOSTTY_OLD="${HOME}/Library/Application Support/com.mitchellh.ghostty/config"
AGENT_SOCK="${HOME}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"

dry_run=0

info() { printf '\033[36m%s\033[0m\n' "$*"; }
warn() { printf '\033[33m%s\033[0m\n' "$*" >&2; }
die() {
  printf '\033[31mERROR: %s\033[0m\n' "$*" >&2
  exit 1
}

run() {
  if ((dry_run)); then
    printf '  + %s\n' "$*"
  else
    "$@"
  fi
}

# -n so relinking a directory replaces the link instead of nesting inside it.
link() {
  local src=${1} dest=${2}
  [[ -e ${src} ]] || {
    warn "skip: ${src} not present"
    return 0
  }
  [[ -L ${dest} && $(readlink "${dest}") == "${src}" ]] && return 0
  run mkdir -p "$(dirname "${dest}")"
  run ln -fns "${src}" "${dest}"
  info "linked ${dest} -> ${src}"
}

profile() {
  [[ -x ${GET_PROFILE} ]] || die "${GET_PROFILE} missing -- run '${0} private-configs' first"
  "${GET_PROFILE}"
}

cmd_bootstrap() {
  command -v brew >/dev/null ||
    run /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  cmd_brew_bundle
  link "${ETC}/mise.toml" "${HOME}/.config/mise/config.toml"
  run mise install
}

cmd_check_deps() {
  local missing=() c
  for c in brew mise op git; do
    command -v "${c}" >/dev/null 2>&1 || missing+=("${c}")
  done
  ((${#missing[@]} == 0)) || die "missing commands: ${missing[*]} -- run '${0} bootstrap' first"

  profile >/dev/null

  op --account my.1password.com vault list >/dev/null 2>&1 || die \
    $'1Password CLI cannot read vaults.\nUnlock the desktop app, then enable Settings > Developer > CLI integration.'

  info "deps ok (profile: $(profile))"
}

cmd_brew_bundle() {
  run env HOMEBREW_AUTO_UPDATE_SECS=9600 brew bundle --file="${EMACS_D}/Brewfile"
}

cmd_clojure_configs() {
  link "${ETC}/clojure" "${HOME}/.clojure"
  link "${ETC}/lein" "${HOME}/.lein"
  # ~/.lein *is* etc/lein, so profiles.clj needs no separate link.
  if [[ $(profile) == work ]]; then
    info "skip clojure-lsp config: dev-env owns it on work machines"
  else
    link "${ETC}/clojure-lsp-config.edn" "${HOME}/.config/clojure-lsp/config.edn"
  fi
}

# Ghostty >=1.1 reads ~/.config/ghostty/config on macOS and follows symlinks.
# Both that and the old Application Support path are read and MERGED, so the
# stale copy has to be moved aside or it silently layers on top of this one.
cmd_ghostty_configs() {
  link "${ETC}/ghostty/config" "${GHOSTTY_XDG}/config"
  link "${ETC}/ghostty/themes" "${GHOSTTY_XDG}/themes"
  if [[ -f ${GHOSTTY_OLD} ]]; then
    run mv -f "${GHOSTTY_OLD}" "${GHOSTTY_OLD}.pre-xdg"
    info "moved obsolete ghostty copy to ${GHOSTTY_OLD}.pre-xdg"
  fi
}

cmd_configs() {
  cmd_clojure_configs
  cmd_ghostty_configs
  link "${ETC}/zshrc" "${HOME}/.zshrc"
  link "${ETC}/zshrc" "${HOME}/.profile"
  link "${ETC}/gitconfig" "${HOME}/.gitconfig"
  link "${ETC}/psqlrc" "${HOME}/.psqlrc"
  link "${ETC}/mise.toml" "${HOME}/.config/mise/config.toml"
}

cmd_private_configs() {
  run git -C "${EMACS_D}" submodule update --init --recursive private-configs
  local branch
  branch=$(git -C "${PRIVATE}" rev-parse --abbrev-ref HEAD)
  if [[ ${branch} != main ]]; then
    warn "private-configs is on '${branch}', not main -- leaving it alone"
  else
    run git -C "${PRIVATE}" pull -r
  fi
  run make -C "${PRIVATE}" setup
}

cmd_op_configs() {
  run make -C "${PRIVATE}" config-op config-ssh
  link "${AGENT_SOCK}" "${HOME}/.config/1Password/agent.sock"
}

cmd_zsh_completion() {
  link /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.zsh "${ZSH_COMP_DIR}/_git"
  link /Applications/Docker.app/Contents/Resources/etc/docker.zsh-completion "${ZSH_COMP_DIR}/_docker"
  link /Applications/Docker.app/Contents/Resources/etc/docker-compose.zsh-completion "${ZSH_COMP_DIR}/_docker-compose"
}

cmd_fix_macos() {
  run defaults write com.apple.finder ShowPathbar -bool true
  run defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
  run defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
}

cmd_setup() {
  cmd_zsh_completion
  cmd_configs
  cmd_op_configs
  cmd_fix_macos
}

cmd_init() {
  cmd_bootstrap
  cmd_private_configs
  cmd_check_deps
  cmd_setup
}

cmd_help() {
  cat <<'EOF'
usage: ./setup.sh [-n|--dry-run] <command>

  init             full bootstrap for a new machine, in dependency order
  setup            everything that needs brew/mise/op to already be present
  bootstrap        brew, the mise config link, then mise install
  check-deps       verify tools, profile and 1Password before doing any work

  configs          zsh/git/psql/mise links, plus the clojure and ghostty sets
  clojure-configs  ~/.clojure, ~/.lein, and clojure-lsp (skipped on work)
  ghostty-configs  ~/.config/ghostty, retiring the Application Support copy
  private-configs  check out the private submodule and run its profile setup
  op-configs       1Password SSH agent config and the agent socket link

  brew-bundle      brew bundle from ./Brewfile
  zsh-completion   link system zsh completions into etc/zsh
  fix-macos        Finder and .DS_Store defaults

Links already pointing at the right target are left alone and print nothing,
so a re-run is quiet.
EOF
}

main() {
  while [[ ${1:-} == -* ]]; do
    case ${1} in
      -n | --dry-run)
        dry_run=1
        info "dry run: no changes will be made"
        shift
        ;;
      -h | --help)
        cmd_help
        return 0
        ;;
      *) die "unknown option: ${1}" ;;
    esac
  done

  local cmd=${1:-help}
  local fn="cmd_${cmd//-/_}"
  if [[ $(type -t "${fn}") == function ]]; then
    "${fn}"
  else
    die "unknown command: ${cmd} (try '${0} help')"
  fi
}

main "$@"
