# Path to aliases
export ZSH_ALIAS="$ZDOTDIR/alias"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

source $ZDOTDIR/zshrc
source $ZSH/oh-my-zsh.sh
source $ZSH_ALIAS/alias.zsh


# typeset -gA key
# key[Up]="${terminfo[kcuu1]}"
# [[ -n "${key[Up]}" ]] && bindkey -- "${key[Up]}" backward-char

# key[Control-Left]="${terminfo[kLFT5]}"
# [[ -n "${key[Control-Left]}" ]] && bindkey -- "${key[Control-Left]}" backward-char
