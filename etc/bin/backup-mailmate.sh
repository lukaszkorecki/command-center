#!/usr/bin/env bash
backupPath=~/Dropbox/AppBackup/Mailmate

mkdir -p "$backupPath"

backup() {
    echo 'baccking up'
    cp -rv	~/Library/Application\ Support/MailMate/*.plist "${backupPath}/"
    cp -rv ~/Library/Preferences/com.freron.MailMate.plist "${backupPath}/"

}
restore() {
    echo 'restoring'
    cp -rv "${backupPath}/*.plist" ~/Library/Application\ Support/MailMate/
    cp -rv "${backupPath}/com.freron.MailMate.plist" ~/Library/Preferences/com.freron.MailMate.plist
}

cmd=${1:-backup}

if [[ "$cmd" = "backup" ]] ; then
    backup
elif [[ "$cmd" == "restore" ]] ; then
    restore
else
    echo "unknown command!: $cmd"
    exit 1
fi
