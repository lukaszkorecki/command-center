
jabba-install() {
  curl -sL https://github.com/shyiko/jabba/raw/master/install.sh | bash && . ~/.jabba/jabba.sh
  jabba install adopt@1.11.28-0
  jabba use adopt@1.11.28-0
}

if [[ -s "/home/vagrant/.jabba/jabba.sh" ]] ; then
  source "/home/vagrant/.jabba/jabba.sh"
  jabba use adopt@1.11.28-0
else
  echo Jabba not installed!
  echo "run jabba-install"
fi
