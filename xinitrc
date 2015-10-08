#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources &

# Fix Java
wmname LG3D &

# Make Skype look prettier
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"

defaults &
backg &
systemctl --user restart redshift

case "$2" in
    xbmc) amixer set Master 100% && exec xbmc-standalone
        ;;
    b) sxhkd &
        xsetroot -cursor_name left_ptr &
        exec bspwm
        ;;
    stumpdev)
        xsetroot -cursor_name left_ptr &
        exec ~/dev/lisp/stumpwm/stumpwm
        ;;
    *)
        xsetroot -cursor_name left_ptr &
        exec stumpwm
        ;;
esac
