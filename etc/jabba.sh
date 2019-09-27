
jabba-install() {
  curl -sL https://github.com/shyiko/jabba/raw/master/install.sh | bash && . ~/.jabba/jabba.sh
  jabba install adopt@1.11.28-0
  jabba use adopt@1.11.28-0
}



if [[ "$(which jabba)" == "" ]] ; then
  echo Jabba not installed!
  echo "run jabba-install"
else
  jabba use adopt@1.11.28-0
fi
