#!/bin/sh


setxkbmap -print | sed -e '/xkb_keycodes/s/"[[:space:]]/+ext&/' | xkbcomp -I$HOME/.config/keyboard - $DISPLAY
