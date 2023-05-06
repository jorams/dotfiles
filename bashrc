# To be included by machine-specific bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Environment variables -------------------------------------------------------

# ~/bin and ~/bin/blob come first so that other executables can be overridden
PATH=~/bin:~/bin/blob:$PATH
export PATH

# Bash settings ---------------------------------------------------------------
complete -cf sudo
set -o emacs
shopt -s globstar

# Aliases
alias ls='ls --indicator-style=slash --color=auto'
alias color='stint | while read -r; do printf "#%X %X %X\n" $REPLY; done'
alias sc='sudo systemctl'
alias scu='systemctl --user'
alias sp='sudo pacman'
alias cls='clear && ls'
alias torrd='systemctl --user start transmission'
alias torr='transgui'

function cd() {
    builtin cd "$1"
    ls
}

# Other program settings ------------------------------------------------------

# .lesshst is useless clutter
export LESSHISTFILE='-'

# GTK refuses to scroll without this
export GDK_CORE_DEVICE_EVENTS=1

# The prompt ------------------------------------------------------------------

if [[ $TERM != dumb ]]; then
    my_prompt () {
        last=$?
        if [[ $last == 0 ]]; then
            pcolor="\[\e[0;34m\]"
        else
            pcolor="\[\033[0;31m\] $last"
        fi

        host=""
        [ -n "$SSH_CONNECTION" ] && host="\h "

        PS1="$pcolor $host» \[\e[0m\]"
    }

    PROMPT_COMMAND='my_prompt'
fi
