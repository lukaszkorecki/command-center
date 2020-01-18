JAVA_VERSION=adopt@1.11.0-5

jabba-install() {
  curl -sL https://github.com/shyiko/jabba/raw/master/install.sh | bash && . ~/.jabba/jabba.sh
  jabba install $JAVA_VERSION
  jabba use $JAVA_VERSION
}

if [[ -s "/home/vagrant/.jabba/jabba.sh" ]] ; then
  source "/home/vagrant/.jabba/jabba.sh"
  jabba use $JAVA_VERSION
fi
