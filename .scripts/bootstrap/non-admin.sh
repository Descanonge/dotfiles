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
    ~/.miniconda3/condabin/conda install python
fi

# Python packages
# Some python packages I use regularly
if confirm "Python science packages"; then
    conda install numpy matplotlib cartopy cftime \
        scipy
fi

# Python own lib
# My own packages
if confirm "Personal python libraries"; then
    git clone http://github.com/Descanonge/mypack \
        ~/Documents/Libraries/Python/MyPack
    pip install -e ~/Documents/Libraries/Python/MyPack
    git clone http://github.com/Descanonge/tomate \
        ~/Documents/Libraries/Python/Tomate
    pip install -e ~/Documents/Libraries/Python/Tomate
fi

# Python checkers
if confirm "Python checkers"; then
    conda install flake8 pylint pytest
fi

# DOOM
if confirm "Doom Emacs"; then
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
fi

# Gsettings
if confirm "dconf dump"; then
    dconf load /org/ < ~/.config/dconf/org-dump.txt
fi

# Tehfuck
if confirm "thefuck"; then
    pip install thefuck
fi

# Argos
if confirm "Argos"; then
    pip install argos
fi
