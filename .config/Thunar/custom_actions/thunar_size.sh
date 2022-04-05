#!/bin/bash

directory=$@

info=$(du -hc -d0 ${directory[@]} | sort -h)


for i in $info; do
    echo $i
done | zenity --list \
    --column="Size" --column="Files" \
    --title="Size" --width="500" --height="600"
