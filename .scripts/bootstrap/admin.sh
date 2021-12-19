#!/usr/bin/env bash

# Parts of setup that require admin privileges
# (user should be part of sudoers group)

install () {
    sudo apt install "$1"
}


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
fi

# DIRENV
if confirm "direnv"; then
    install direnv
fi

# VIM
if confirm "vim"; then
    install vim
fi

# Theme
# Install Adapta theme and Papirus icons
# Install a variation of Adapta that does not have window titlebars
if confirm "Adapta theme and icons"; then
    install adapta-gtk-theme papirus-icon-theme
    backup "/usr/share/themes/Adapta/metacity-1/metacity-theme-3.xml"
    sudo cp ~/.config/theme/metacity-theme-3.xml \
        /usr/share/themes/Adapta/metacity-1/metacity-theme-3.xml
fi

# Lightdm Greeter
if confirm "Lightdm Greeter"; then
    sudo cp "$HOME/.config/lightdm/lightdm*.conf" "/etc/lightdm/"
fi

# Thunar
# With archive plugin
if confirm "Thunar"; then
    install thunar thunar-data thunar-archive-plugin
fi

# Nextcloud Client
if confirm "Nextcloud"; then
    install nextcloud-desktop
fi

# Firefox
if confirm "firefox"; then
    install firefox-esr
fi

# # Clementine
# # Also install a custom set of icons
# if confirm "Clementine"; then
#     install clementine
#     git clone https://github.com/narunlifescience/Clementine-Custom-Icon-Sets.git \
#         "$HOME/Downloads/clementine-iconset"
#     mkdir -p ~/.config/Clementine/customiconset
#     cp -r ~/Downloads/clementine-iconset/FaenzaLight/* ~/.config/Clementine/customiconset/
# fi

# Audio server
# To stream audio to local network
# See .scripts/start_audio_stream.sh and .local/share/applications/Stream.desktop
if confirm "audio server"; then
    sudo debconf-set-selections "$HOME/.config/audio_stream/icecast2.conf"
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
    tar -xf ~/Downloads/zotero.tar.bz2 -C ~/Downloads/
    sudo cp -r ~/Downloads/Zotero_linux-x86_64 /opt/zotero
    sudo /opt/zotero/set_launcher_icon
fi

# Mail Stuff
if confirm "Mails"; then
    install mbsync mailnag notmuch
fi
