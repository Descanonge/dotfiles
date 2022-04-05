#!/bin/bash

# Rsync directory with target folder
# Target folder is asked, PARENT FOLDER IS USED
# If Milton or Passport is connected, try to find arborescence

in="$1"

try="$HOME"

if [ -e /media/clement/Milton ]; then
	root=$(realpath --relative-to="$HOME" "$in")
	try="/media/clement/Milton/$root"
fi

if [ -e /media/clement/Melentia ]; then
	root=$(realpath --relative-to="$HOME" "$in")
	try="/media/clement/Melentia/$root"
fi

target=$(zenity --file-selection --filename="$try" --directory)
if [[ "$target" == "" ]]; then
	exit 1
fi
parent="$(dirname "$target")"

tmp=$(mktemp)
synctxt=$(rsync -arvh --delete --only-write-batch="$tmp" "$in" "$parent")
echo -e "$synctxt" | zenity --text-info --ok-label="Confirm" --cancel-label="Cancel" \
	--width=500 --height=600
if [ $? = 0 ]; then
	bash "$tmp.sh"
	notify-send "Syncing terminated" "$in" --icon="emblem-synchronizing-symbolic"
fi

rm "$tmp"
