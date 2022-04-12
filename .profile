# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022


# Locales
export LANG=en_US.utf8
export LC_MONETARY=fr_FR.utf8
export LC_MEASUREMENT=fr_FR.utf8
export LC_NUMERIC=en_US.utf8
export LC_PAPER=fr_FR.utf8
export LC_TELEPHONE=fr_FR.utf8
export LC_TIME=fr_FR.utf8


# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi


# if running zsh
if [ -n "$ZSH_VERSION" ]; then
    if [ -f "$HOME/.zsh/.zshrc" ]; then
        . "$HOME/.zsh/.zshrc"
    fi
fi


# Add conda to PATH
if [ -d "$HOME/.miniconda3/envs/default/bin" ] ; then
    PATH="$PATH:$HOME/.miniconda3/envs/default/bin"
fi
if [ -d "$HOME/.miniconda3/bin" ] ; then
    PATH="$PATH:$HOME/.miniconda3/bin"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export XDG_CONFIG_HOME="$HOME/.config"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/config"
export MAILDIR="$HOME/mail"

export PYTHONPATH="$HOME/.miniconda3/pythonpath"

export SUBMESO_COLOR_CODE_DIR="$HOME/Documents/Work/Fronts"
export SUBMESO_COLOR_DATA_DIR="$HOME/Documents/Work/Data"

export EXPERIMENTS_DIR="$HOME/Documents/Work/Experiments"
