#!/usr/bin/env bash

confirm () {
    target=$1
    read -p "Install $target ? (y/N)" -n 1 -r
    echo    # move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        return 0
    else
        return 1
    fi
}

# Backup files. If backup already exists, restore them.
backup () {
    for file in "$@"; do
        bak="${file}.bak"
        if [ ! -f "$bak" ]; then
            sudo cp "$file" "$bak"
        else
            sudo cp "$bak" "$file"
        fi
    done
}
