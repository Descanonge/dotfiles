#!/bin/bash

# Archive emails by year
year=2019
dry=0

IFS=$'\n'
files=($(notmuch search --output=files not tag:inbox not tag:flagged and date:.."$year"1231))
unset IFS

for f in "${files[@]}"; do
    f_rel="$(realpath --relative-to "$MAILDIR/archives" "$f")"
    target_base="$(basename "$f")"
    target_dir="$MAILDIR/vault/$year/$(dirname "$f_rel")"
    target_file="$target_dir/$target_base"

    if [ ! -d "$target_dir" ]; then
        echo "Creating target directory $target_dir"
        mkdir -p "$target_dir"
    fi

    if [ -f "$target_file" ]; then
        echo "File already exist $target_file"
    else
        if [ "$dry" == 0 ]; then
            echo "Moving $f to $target_file"
            mv "$f" "$target_file"
        else
            echo "Would move $f to $target_file"
        fi
    fi

done
