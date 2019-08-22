export DEBIAN_FRONTEND=noninteractive

echo "Etc/UTC" > /etc/timezone
sudo dpkg-reconfigure -f noninteractive tzdata


sudo apt install -y apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"

curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -

sudo apt -y update

sudo apt install -y yarn docker-ce \
     emacs25 emacs25-common \
     tmux curl ruby2.5 ruby2.5-dev \
     zlib1g-dev liblzma-dev \
     build-essential patch \
     openjdk-8-jre-headless \
     libpq-dev \
     google-cloud-sdk

sudo gem install bundler -v 1.17.1


curl -s https://raw.githubusercontent.com/envkey/envkey-source/master/install.sh | bash
