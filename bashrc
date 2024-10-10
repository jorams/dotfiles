# To be included by machine-specific bashrc

# Environment variables -------------------------------------------------------

# ~/bin and ~/bin/blob come first so that other executables can be overridden
export PATH=~/bin:~/bin/blob:$PATH

# If not running interactively, don't do anything else
[[ $- != *i* ]] && return

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

# Elixir Plug can generate links to files in debugger views
# Requires emacsclient-from-url to be set up
export PLUG_EDITOR="emacsclient:__FILE__:__LINE__"

# Use gcr-ssh-agent for SSH
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"

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
