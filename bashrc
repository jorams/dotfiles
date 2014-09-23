#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

eval `dircolors ~/.dir_colors`

#--alias
alias ls='ls --indicator-style=slash --color=auto'
alias color='stint | while read -r; do printf "#%X %X %X\n" $REPLY; done'
alias sshot='scrot --exec '\''mv $f ~/pictures/screenshots'' 2>/dev/null'\'
alias sc='sudo systemctl'
alias sp='sudo pacman'
alias cls='clear && ls'
alias db='dropbox'
alias torrd='sudo systemctl start transmission'
alias torr='transmission-remote-cli'
alias calc='bc -i'
alias mh='mpv --ao=alsa:device=[plughw:0,3]'
alias genpass='pwgen -s -y 16 1 | xclip -selection CLIPBOARD'
alias irctunnel='ssh torrn -L localhost:2345:localhost:2345'

function cd() {
    builtin cd "$1"
    ls
}

#-- Alternative binary paths
PATH=$PATH:~/bin:~/.cabal/bin:~/.gem/ruby/2.1.0/bin:~/dev/php/arcanist/bin
export PATH

#--Get rid of .lesshst, useless clutter
export LESSHISTFILE='-'

complete -cf sudo
set -o vi
shopt -s globstar

if [ `id -u` -eq "0" ]; then
	userp="»"
else
	userp=""
fi

PS1="\[\e[0;34m\] » ${userp} \[\e[0m\]"
