#!/usr/bin/env bash

BOOTSTRAP="$(realpath "$(dirname "${BASH_SOURCE[0]}")")"
. "$BOOTSTRAP/confirm.sh"


# Keyboard
if confirm "custom Neo keyboard layout"; then
    source "$BOOTSTRAP/keyboard.sh" "$BOOTSTRAP"
fi

# SYS PROGRAMS
if confirm "software with apt"; then
    source "$BOOTSTRAP/apt.sh" "$BOOTSTRAP"
fi

source "$BOOTSTRAP/other.sh" "$BOOTSTRAP"


# From HDD
if confirm "data from external disk ?"; then
   source "$BOOTSTRAP/from_hdd.sh" "$BOOTSTRAP"
fi
