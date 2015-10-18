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

defaults &
backg &
systemctl --user restart redshift

xsetroot -cursor_name left_ptr &
case "$2" in
    ddev)
        exec ~/dev/lisp/stumpwm/stumpwm
        ;;
    edev)
        exec ~/dev/stumpwm/stumpwm/stumpwm
        ;;
    *)
        exec stumpwm
        ;;
esac
