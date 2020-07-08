#!/bin/bash

# Execute rsync cmd

args=("$@")

tmp=$(mktemp)
synctxt=$(rsync -arvh --delete --only-write-batch="$tmp" "${args[@]}")
echo -e "$synctxt" | zenity --text-info --ok-label="Confirm" --cancel-label="Cancel" \
	--width=500 --height=600
if [ $? = 0 ]; then
	bash "$tmp.sh"
fi


rm $tmp
