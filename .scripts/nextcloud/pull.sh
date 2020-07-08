#!/bin/bash

# Pull argument files or directory from nextcloud

files=("$@")
dir=$(dirname ${files[0]})
target="$(realpath --relative-to="$HOME" $dir)"
target="$HOME/$target"

files=($(realpath --relative-to="$HOME" ${files[@]}))

for ((i=0; i<${#files[@]}; i++)); do
	files[$i]="$HOME/Nextcloud/${files[$i]}"
done

bash ~/.scripts/nextcloud/execute.sh "${files[@]}" "$target"
