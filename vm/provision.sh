#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive

log() {
  logger -t SETUP -s "$*"
}

if [[ $(cat /etc/timezone) != "Etc/UTC" ]] ; then
  log "Setting up timezone to UTC"
  echo "Etc/UTC" > /etc/timezone
  sudo dpkg-reconfigure -f noninteractive tzdata
  sudo timedatectl set-ntp false
  sudo apt install -y ntp
else
  log "Timezone is in UTC"
fi

if [[ "$(which keychain)" == "" ]] ; then
  log "Installing dev tools and runtimes"
  sudo apt install -y apt-transport-https ca-certificates software-properties-common \
       emacs25 emacs25-common \
       tmux curl jq \
       ruby2.5 ruby2.5-dev \
       zlib1g-dev liblzma-dev \
       build-essential patch \
       libpq-dev \
       bash-completion \
       keychain
else
  log "Dev tools installed"
fi

if [[ "$(which java)" == "" ]] ; then
  log "Installing java"
  curl -fsSL https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public | sudo apt-key add -
  sudo add-apt-repository --yes https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/
  sudo apt-get update
  sudo apt-get install -y  adoptopenjdk-11-hotspot
else
  log "Java installed already"
fi

if [[ "$(which gcloud)" == "" ]] ; then
  log "Installing gcloud, yarn, docker"
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
  sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"

  curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
  echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
  echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

  curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -

  sudo apt -y update

  sudo apt install -y yarn docker-ce \
       google-cloud-sdk

  sudo usermod -aG docker vagrant
  sudo usermod -aG docker ubuntu
  sudo shutdown -r now

else
  log "Gcloud/yarn/docker already installed"
fi

if [[ "$(which bundler)" == "" ]] ; then
  log "installing bundler"
  sudo gem install bundler -v 1.17.1
else
  log "bundler already installed"
fi

if [[ "$(which envkey-source)" == "" ]] ; then
  log "installing envkey"
  curl -s https://raw.githubusercontent.com/envkey/envkey-source/master/install.sh | bash
else
  log "envkey installled"
fi

if [[ "$(which docker-compose)" == "" ]] ; then
  log "installing docker compose"
  sudo curl -L "https://github.com/docker/compose/releases/download/1.24.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
  sudo chmod +x /usr/local/bin/docker-compose
else
  log "docker compose already installed"
fi
