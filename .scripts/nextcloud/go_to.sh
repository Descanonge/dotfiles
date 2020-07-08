#!/bin/bash

# Switch to corresponding Nextcloud folder

folder=$1

path=$(realpath --relative-to="$HOME" $folder)

root=$(echo $path | cut -d '/' -f1)

if [ "$root" == "Nextcloud" ]; then
	path="$HOME/${path#*/}"
else
	path="$HOME/Nextcloud/$path"
fi

while true; do [ -e "$path" ] && break || path=$(dirname "$path"); done

thunar $path
