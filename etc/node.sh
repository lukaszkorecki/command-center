activateNode() {
  case "$1" in
    10)
      export PATH=~/bin/node/bin:$PATH
      node --version
      ;;
    14)
      export PATH=~/bin/node-14/bin:$PATH
      node --version
      ;;
    *)
      echo 'select a node version!'
      ;;
  esac
}

echo 'nodejs not activated, use activateNode' > /dev/stderr
