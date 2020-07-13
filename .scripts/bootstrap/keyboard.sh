#!/usr/bin/env bash
# Bootstrap custom Neo layout

KEYCONFIG="$HOME/.config/keyboard/layout"
RULE="$KEYCONFIG/evdev.xml"
LAYOUT="$KEYCONFIG/de"

TARGET="/usr/share/X11/xkb"
DEFAULT="/etc/default/keyboard"

files="$TARGET/symbols/de $TARGET/rules/evdev.xml $DEFAULT"

for file in "${files[@]}"; do
    if [ ! -f "$file.bak" ]; then
        sudo cp "$file"  "$file.bak"
    else
        sudo cp "$file.bak" "$file"
    fi
done


sudo tee -a "$TARGET/symbols/de" < "$LAYOUT"

line_s="$(cat "$TARGET/rules/evdev.xml" | grep -nE '<description>German<' \
    | cut -d':' -f1)"
line_p="$(tail +"$line_s" "$TARGET/rules/evdev.xml" | grep -nE '<variantList>' \
    | head -n1 | cut -d':' -f1)"
line="$((line_s+line_p-1))"
sudo sed -i "${line}r$RULE" "$TARGET/rules/evdev.xml"

sudo sed -i 's/\(XKBLAYOUT\)="\(.*\)"/\1="de"/' $DEFAULT
sudo sed -i 's/\(XKBVARIANT\)="\(.*\)"/\1="neofr"/' $DEFAULT
