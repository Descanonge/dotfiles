# Path to aliases
export ZSH_ALIAS="$ZDOTDIR/alias"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"
export ZSH_CUSTOM="$ZDOTDIR/oh-my-zsh"

# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="agnosterer"
DEFAULT_USER="clement"

VIRTUAL_ENV_DISABLE_PROMPT="False"
CONDA_DEFAULT_ENV_HIDE="default"

# COMPLETION_WAITING_DOTS="true"

plugins=(colorize
         copypath
         jump
         pip
         zsh-autosuggestions
        )

# typeset -gA key
# key[Up]="${terminfo[kcuu1]}"
# [[ -n "${key[Up]}" ]] && bindkey -- "${key[Up]}" backward-char

# key[Control-Left]="${terminfo[kLFT5]}"
# [[ -n "${key[Control-Left]}" ]] && bindkey -- "${key[Control-Left]}" backward-char

# Ruby gems
export GEM_HOME="$HOME/.gems"
export PATH="$PATH:$HOME/.gems/bin"

# colored ls commands
if [ -f "$HOME/.dircolors.sh" ]; then
   eval $(dircolors -b $HOME/.dircolors.sh)
fi

export MAMBA_NO_BANNER='yeah'

# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba init' !!
export MAMBA_EXE='/home/clement/.local/bin/micromamba';
export MAMBA_ROOT_PREFIX='/home/clement/.micromamba';
__mamba_setup="$("$MAMBA_EXE" shell hook --shell zsh --root-prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    alias micromamba="$MAMBA_EXE"  # Fallback on help from mamba activate
fi
unset __mamba_setup
# <<< mamba initialize <<<

micromamba activate "$CONDA_DEFAULT_ENV_HIDE"

# eval $(thefuck --alias)
eval "$(direnv hook zsh)"

# Autocomplete ssh / scp / ftp commands from ssh config file
[ -r ~/.ssh/config ] && _ssh_config_hosts=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p')) || _ssh_config_hosts=()
[ -r ~/.ssh/config ] && _ssh_config_users=($(cat ~/.ssh/config | sed -ne 's/User[=\t ]//p')) || _ssh_config_users=()
zstyle ':completion:*:(ssh|scp|ftp|sftp):*' hosts $_ssh_config_hosts
zstyle ':completion:*:(ssh|scp|ftp|sftp):*' users $_ssh_config_users

export FC=gfortran
export CC=gcc
export CXX=g++
# export OCSSWROOT="$HOME/.ocssw"
# source $OCSSWROOT/OCSSW_bash.env

source $ZSH/oh-my-zsh.sh
source $ZSH_ALIAS/alias.zsh
