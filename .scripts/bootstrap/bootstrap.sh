#!/usr/bin/env bash

# Bootstrap script that will install I need

BOOTSTRAP="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
. "$BOOTSTRAP/utilities.sh"


echo "Sourcing .profile"
source "$HOME/.profile"


# Install submodules
echo "Updating submodules"
yadm submodule update --init --recursive

# Install custom keyboard layout
if confirm "custom Neo keyboard layout"; then
    source "$BOOTSTRAP/keyboard.sh" "$BOOTSTRAP"
fi

# Install stuff that need admin privileges
if confirm "software with apt"; then
    source "$BOOTSTRAP/admin.sh" "$BOOTSTRAP"
fi

# Install stuff that does not need admin privileges
source "$BOOTSTRAP/non-admin.sh" "$BOOTSTRAP"

# Copy some folders from external disk
if confirm "data from external disk ?"; then
   source "$BOOTSTRAP/from_disk.sh" "$BOOTSTRAP"
fi
