#!/bin/bash

# Switch to corresponding pCloud folder

folder=$1

path=$(realpath --relative-to="$HOME" $folder)

root=$(echo $path | cut -d '/' -f1)

if [ "$root" == "pCloudDrive" ]; then
	path="$HOME/${path#*/}"
else
	path="$HOME/pCloudDrive/$path"
fi

while true; do [ -e "$path" ] && break || path=$(dirname "$path"); done

thunar $path
