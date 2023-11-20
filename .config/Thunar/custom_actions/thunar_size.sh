#!/bin/bash

du -hc --null -d0 "$@" \
    | sort -hz \
    | tr \\t \\0 \
    | zenity --list \
    --column="Size" --column="Files" \
    --title="Size" --width="500" --height="600"
