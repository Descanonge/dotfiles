#!/usr/bin/env bash

# Move stuff from HDD if connected

HDD="/media/clement/Melentia/"

read -p "HDD is correct ? [$HDD] Press enter to confirm or enter alternative.
" -r
if [[ ! $REPLY == "" ]]; then
    HDD=$REPLY
fi

if [[ ! -d "$HDD" ]] ; then
    echo "$HDD" is not a valid directory. Aborting...
    exit 1
fi


# Thunderbird
if confirm "thunderbird"; then
    cp "$HDD/Config/.thunderbird" "$HOME/"
fi

# SSH
if confirm "ssh keys"; then
    cp "$HDD/Config/.ssh" "$HOME/"
fi

# Fonts
if confirm "fonts"; then
    cp "$HDD/Config/.fonts" "$HOME/"
fi

# Wallpapers
if confirm "wallpapers"; then
   cp "$HDD/Pictures/Wallpapers" "$HOME/Pictures/"
fi
