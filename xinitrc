#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -z "$XR_FONT" ]] && export XR_FONT="-*-tamsyn-medium-r-*-*-20-*-*-*-*-*-*-*"
[[ -f ~/.Xresources ]] && xrdb -DXR_FONT="$XR_FONT" -load ~/.Xresources &

# Fix Java
wmname LG3D &

defaults &
systemctl --user restart redshift

export GTK_THEME=Arc:dark

xsetroot -cursor_name left_ptr &
case "$2" in
    dev)
        exec ~/dev/stumpwm/stumpwm/stumpwm
        ;;
    dev-ql)
        exec sbcl --eval '(ql:quickload :stumpwm)' --eval '(stumpwm:stumpwm)' --quit
        ;;
    *)
        exec stumpwm
        ;;
esac
