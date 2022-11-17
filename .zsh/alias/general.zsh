
alias v="vim"
alias j="jump"

alias l="/usr/bin/ls --color=auto -p"
alias la="/usr/bin/ls --color=auto -pA"
alias ll="/usr/bin/ls --color=auto -plh"
alias lla="/usr/bin/ls --color=auto -plAh"

alias fd="fdfind"

alias sz="du -hc -d1 . | sort -h"

alias -g L="| less"
alias -g T="| tail"

alias py="ipython --config $HOME/.ipython/profile_default/ipython_fast.py"

mkcdir () {
    mkdir -p -- "$1" &&
        cd -P -- "$1"
}
