#!/bin/bash

# Send files or download from pCloud folder.

pcloud="$HOME/pCloudDrive"

if ! cd "$pcloud"; then
    echo "Could not find Nextcloud directory"
    exit 1
fi

files=("$@")

testfile="${files[0]}"
normfile="$(realpath "$testfile")"
relfile="$(realpath --relative-base="$pcloud" "$testfile")"


# If inside Nextcloud directory
if [[ "$normfile" != "$relfile" ]]; then
    # PULL
    echo "PULL"
    sourcedir="$(dirname "$(realpath --relative-to="$pcloud" "$testfile")")"
    target="$HOME/$sourcedir"
else
    # PUSH
    echo "PUSH"
    sourcedir="$(dirname "$(realpath --relative-to="$HOME" "$testfile")")"
    target="$pcloud/$sourcedir"
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
