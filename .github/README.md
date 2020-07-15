
# Dotfiles

Managed with [Yadm](http://yadm.io).

## Install
``` sh
git clone https://github.com/TheLocehiliosan/yadm.git ~/.yadm-project
~/.yadm-project/yadm clone https://github.com/Descanonge/dotfiles
yadm bootstrap
```

## Bootstrap
Comes with a boostrap bash script found in [.scripts/bootstrap](../scripts/bootstrap).
It will (after prompting user for confirmation before doing anything):
- install a new custom keyboard layout derived from [Neo](http://neo-layout.org),
  with french accents (éèêëù) instead of some german specific letters.
  It also put escape on the spacebar in the fourth layer (instead of zero).
  It will run dpkg-reconfigure keyboard-configuration to make it a system default.
- set up locales
- install a variety of packages using aptitude
- various setups that do not require admin privileges
  (Miniconda, Doom Emacs, OhMyZsh, Gsettings config,...)
- copy some folders from a external drive (fonts, wallpapers, mails,...)
