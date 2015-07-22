# To be included by machine-specific bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#--alias
alias ls='ls --indicator-style=slash --color=auto'
alias color='stint | while read -r; do printf "#%X %X %X\n" $REPLY; done'
alias sshot='scrot --exec '\''mv $f ~/pictures/screenshots'' 2>/dev/null'\'
alias sc='sudo systemctl'
alias sp='sudo pacman'
alias cls='clear && ls'
alias torrd='systemctl --user start transmission'
alias torr='transgui'

function cd() {
    builtin cd "$1"
    ls
}

#-- Alternative binary paths
PATH=$PATH:~/bin:~/bin/blob:~/.cabal/bin
export PATH

#--Get rid of .lesshst, useless clutter
export LESSHISTFILE='-'

complete -cf sudo
set -o emacs
shopt -s globstar

envoy -t ssh-agent id_ecdsa
source <(envoy -p)

my_prompt () {
    last=$?
    if [[ $last == 0 ]]; then
        pcolor="\[\e[0;34m\]"
    else
        pcolor="\[\033[0;31m\] $last"
    fi

    PS1="$pcolor » \[\e[0m\]"
}

PROMPT_COMMAND='my_prompt'
