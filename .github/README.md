
# Dotfiles

Managed with [Yadm](http://yadm.io).

## Install
``` sh
sudo apt install yadm
yadm clone https://github.com/Descanonge/dotfiles
```

## Bootstrap
Yadm should automatically launch `~/.config/yadm/bootstrap`.
- install a new custom keyboard layout derived from [Neo](http://neo-layout.org), with french accents (éèêëù) instead of some german specific letters. It also put escape on the spacebar in the fourth layer (instead of zero).
  It will run dpkg-reconfigure keyboard-configuration to make it a system default.
- set up locales
- install a variety of packages using aptitude and setup some stuff (install OhMyZsh, Zotero, Mambaforge, build Emacs + Doom)
