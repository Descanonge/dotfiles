#!/usr/bin/env bash

install () {
    soft="$1"
    sudo apt install "$soft"
}


# LOCALES
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
fi

# DIRENV
if confirm "direnv"; then
    install direnv
fi

# VIM
if confirm "vim"; then
    install vim
fi

# EMACS
if confirm "emacs"; then
    install emacs
fi

# Theme
if confirm "Adapta theme and icons"; then
    install adapta-gtk-theme papirus-icon-theme
    file="/usr/share/themes/Adapta/metacity-1/metacity-theme-3.xml"
    if [ ! -f "$file.bak" ]; then
        sudo cp "$file" "$file.bak"
    else
        sudo cp "$file.bak" "$file"
    fi
    sudo cp ~/.config/theme/metacity-theme-3.xml \
        /usr/share/themes/Adapta/metacity-1/metacity-theme-3.xml
fi

# Nextcloud Client
if confirm "Nextcloud"; then
    install nextcloud-desktop
fi

# Firefox
if confirm "firefox"; then
    install firefox-esr
fi

# Thunderbird
if confirm "thunderbird"; then
    install thunderbird
fi

# Clementine
if confirm "Clementine"; then
    install clementine
    git clone https://github.com/narunlifescience/Clementine-Custom-Icon-Sets.git \
        "$HOME/Downloads/clementine-iconset"
    mkdir -p ~/.config/Clementine/customiconset
    cp -r ~/Downloads/clementine-iconset/FaenzaLight/* ~/.config/Clementine/customiconset/
fi
# Audio server
if confirm "audio server"; then
    install icecast2 darkice
fi

# Audacity
if confirm "audacity"; then
    install audacity
fi

# Quodlibet
if confirm "QuodLibet"; then
    install quodlibet
fi

# Zeal
if confirm "zeal"; then
    install zeal
fi

# Zotero
if confirm "zotero"; then
    wget "https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64" \
        -O ~/Downloads/zotero.tar.bz2
    tar -xf ~/Downloads/zotero.tar.bz2
    sudo cp -r ~/Downloads/Zotero_linux-x86_64 /opt/zotero
    /opt/zotero/set_launcher_icon
fi
