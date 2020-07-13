#!/usr/bin/env bash

confirm () {
    target=$1
    read -p "Install $target ? (y/N)" -n 1 -r
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        return 0
    else
        return 1
    fi
}
