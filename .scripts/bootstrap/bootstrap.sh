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

# LOCALES
# Install English and french locales
if confirm "locales"; then
    echo "Europe/Paris" | sudo tee /etc/timezone
    sudo dpkg-reconfigure -f noninteractive tzdata
    sudo sed -i -e 's/# en_US.UTF-8 UFT-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
    sudo sed -i -e 's/# fr_FR.UTF-8 UFT-8/fr_FR.UTF-8 UTF-8/' /etc/locale.gen
    sudo dpkg-reconfigure --frontend=noninterative locales
    sudo update-locale LANG=en_US.UTF-8
fi

# ZSH
if confirm "zsh"; then
    install zsh
    export ZDOTDIR="$HOME/.zsh"
    export ZSH="$ZDOTDIR/oh-my-zsh"
    sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# DIRENV
if confirm "direnv"; then
    install direnv
fi

# VIM
if confirm "vim"; then
    install vim
    mkdir -p "$HOME/.vim/swapfiles"
fi

# DOOM
if confirm "Doom Emacs"; then
    git clone --depth 1 https://github.com/Descanonge/doom-emacs ~/.emacs.d
    git remote add upstream "git@github.com:hlissner/doom-emacs"
    ~/.emacs.d/bin/doom install
    # Setup custom parrot
    cp -r "$HOME/.emacs/.local/straight/parrot/img/default" \
        "$HOME/.doom.d/parrot/img/default"
    convert -size 28x20 xc:transparent \
        "$HOME/.doom.d/parrot/img/default/default-parrot-frame-1.xpm"
fi


# Copy some folders from external disk
if confirm "data from external disk ?"; then
   source "$BOOTSTRAP/from_disk.sh" "$BOOTSTRAP"
fi
