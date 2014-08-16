function cls
    clear; ls $argv
end

function cd
    builtin cd $argv[1]
    ls $argv[2..$argc]
end

function ccd
    clear
    cd $argv
end

function fish_prompt
    set -l last_status $status
    if [ $last_status -ne 0 ]
        printf "%s %d" (set_color red --bold) $last_status
    end
    printf "%s" (set_color blue)
    printf " » "
end

function torrd
    sudo systemctl start transmission
end

function torr
    transmission-remote-cli
end

function sc
    sudo systemctl $argv
end

function sp
    sudo pacman $argv
end

function db
    dropbox $argv
end

function color
    bash -c 'stint | while read -r; do printf "#%X %X %X\n" $REPLY; done'
end

function mh
    mpv --ao=alsa:device=[plughw:0,3] $argv
end

function genpass
    pwgen -s -y 16 1 | xclip -selection CLIPBOARD
end

set -U fish_greeting ""
set -U EDITOR "vim"
