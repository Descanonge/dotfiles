#!/bin/bash

# Send files or download from Nextcloud folder.

files=("$@")

if [[ ${files[0]} == $HOME/Nextcloud* ]]; then
    script="$HOME/.scripts/nextcloud/pull.sh"
else
    script="$HOME/.scripts/nextcloud/push.sh"
fi

bash $script ${files[@]}
