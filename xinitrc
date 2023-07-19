#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xr_font="xft:Iosevka Output Extended:pixelsize=14"
if [ -n "$IS4K" ]; then
    export GDK_SCALE=2
    export QT_SCALE_FACTOR=2
    xr_font="xft:Iosevka Output Extended:pixelsize=18"
fi

[[ -f ~/.Xresources ]] && xrdb -DXR_FONT="$xr_font" -load ~/.Xresources &

# Fix Java
wmname LG3D &

defaults &
systemctl --user restart redshift

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK
systemctl --user import-environment SSH_AUTH_SOCK

command -v qt5ct && export QT_QPA_PLATFORMTHEME=qt5ct
command -v dunst && systemctl --user start dunst.service

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
