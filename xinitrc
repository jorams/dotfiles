#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

if [ -n "$IS4K" ]; then
    export GDK_SCALE=2
    export QT_SCALE_FACTOR=2
    export XR_FONT="-*-tamsyn-medium-r-*-*-40-*-*-*-*-*-*-*"
fi

[[ -z "$XR_FONT" ]] && export XR_FONT="-*-tamsyn-medium-r-*-*-20-*-*-*-*-*-*-*"
[[ -f ~/.Xresources ]] && xrdb -DXR_FONT="$XR_FONT" -load ~/.Xresources &

# Fix Java
wmname LG3D &

defaults &
systemctl --user restart redshift
init-audio

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

command -v qt5ct && export QT_QPA_PLATFORMTHEME=qt5ct

xsetroot -cursor_name left_ptr &
case "$2" in
    dev)
        exec env SBCL_HOME=/usr/lib/sbcl/ ~/dev/stumpwm/stumpwm/stumpwm
        ;;
    dev-ql)
        exec sbcl --eval '(ql:quickload :stumpwm)' --eval '(stumpwm:stumpwm)' --quit
        ;;
    *)
        exec env SBCL_HOME=/usr/lib/sbcl/ stumpwm
        ;;
esac
