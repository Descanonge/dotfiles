#!/bin/bash

# Push argument files or directory to nextcloud

files=("$@")
dir=$(dirname ${files[0]})
target="$(realpath --relative-to="$HOME" $dir)"
target="$HOME/Nextcloud/$target"

bash ~/.scripts/nextcloud/execute.sh "${files[@]}" "$target"


