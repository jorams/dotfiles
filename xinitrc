#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources &
test -f ~/.Xkeymap && xkbcomp ~/.Xkeymap $DISPLAY

# Update fonts
xset +fp /usr/share/fonts/local
xset fp rehash

# Fix Java
wmname LG3D &
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

# screen off after 5 minutes
xset +dpms
xset dpms 0 0 300

backg &
doset &

# Make Skype look prettier
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"

case "$2" in
    xbmc) amixer set Master 100% && exec xbmc-standalone
        ;;
    xmonad) xsetroot -cursor_name left_ptr &
        exec /usr/bin/xmonad
        ;;
    b) sxhkd &
        xsetroot -cursor_name left_ptr &
        exec bspwm
        ;;
    evlwm)
        xsetroot -cursor_name left_ptr &
        exec sbcl --eval '(ql:quickload :evlwm :verbose t)' --eval '(evlwm:evlwm)' --quit
        ;;

    stumpl)
        xsetroot -cursor_name left_ptr &
        exec sbcl --eval '(ql:quickload :stumpwm :verbose t)' --eval '(stumpwm:stumpwm)' --quit
        ;;
    stumpecl)
        xsetroot -cursor_name left_ptr &
        exec ecl -eval '(ql:quickload :stumpwm :verbose t)' -eval '(stumpwm:stumpwm)'
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
