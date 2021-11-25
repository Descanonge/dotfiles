#!/usr/bin/env bash

# Software that does not need sudo rights for installation


# OhMyZsh
if confirm "OhMyZsh"; then
    sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# VIM
echo "Setting up VIM"
mkdir -p "$HOME/.vim/swapfiles"

# PYTHON
# Install Miniconda for Linux
if confirm "Miniconda"; then
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
        -O ~/Downloads/miniconda_installer.sh
    bash ~/Downloads/miniconda_installer.sh -b -p "$HOME/.miniconda3"
    ~/.miniconda3/condabin/conda env create -f ~/.conda/default.yml

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

# Gsettings
if confirm "dconf dump"; then
    dconf load /org/ < ~/.config/dconf/org-dump.txt
fi
