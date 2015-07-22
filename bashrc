#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#--alias
alias ls='ls --indicator-style=slash --color=auto'
alias color='stint | while read -r; do printf "#%X %X %X\n" $REPLY; done'
alias sshot='scrot --exec '\''mv $f ~/pictures/screenshots'' 2>/dev/null'\'
alias sc='sudo systemctl'
alias sp='sudo pacman'
alias cls='clear && ls'
alias db='dropbox'
alias torrd='systemctl --user start transmission'
alias torr='transgui'
alias calc='bc -i'
alias mh='mpv --ao=alsa:device=[plughw:0,3]'
alias genpass='pwgen -s -y 16 1 | xclip -selection CLIPBOARD'
alias irctunnel='ssh torrn -L localhost:2345:localhost:2345'

function cd() {
    builtin cd "$1"
    ls
}

#-- Alternative binary paths
PATH=$PATH:~/bin:~/bin/blob:~/.cabal/bin:~/.gem/ruby/2.1.0/bin:~/dev/php/arcanist/bin
export PATH

#--Get rid of .lesshst, useless clutter
export LESSHISTFILE='-'

complete -cf sudo
set -o vi
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
