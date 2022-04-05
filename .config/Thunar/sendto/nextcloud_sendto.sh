#!/bin/bash

# Send files or download from Nextcloud folder.

nextcloud="$HOME/Nextcloud"

if ! cd "$nextcloud"; then
    echo "Could not find Nextcloud directory"
    exit 1
fi

files=("$@")

testfile="${files[0]}"
normfile="$(realpath "$testfile")"
relfile="$(realpath --relative-base="$nextcloud" "$testfile")"


# If inside Nextcloud directory
if [[ "$normfile" != "$relfile" ]]; then
    # PULL
    echo "PULL"
    sourcedir="$(dirname "$(realpath --relative-to="$nextcloud" "$testfile")")"
    target="$HOME/$sourcedir"
else
    # PUSH
    echo "PUSH"
    sourcedir="$(dirname "$(realpath --relative-to="$HOME" "$testfile")")"
    target="$nextcloud/$sourcedir"
fi

echo "$target"
echo "${files[@]}"

tmp=$(mktemp)
synctxt=$(rsync -arvh --delete --only-write-batch="$tmp" "${files[@]}" "$target")
echo -e "$synctxt" | zenity --text-info --ok-label="Confirm" --cancel-label="Cancel" \
    --width=500 --height=600
if [ $? = 0 ]; then
    bash "$tmp.sh"
fi

rm "$tmp"
