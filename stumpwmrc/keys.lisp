(in-package #:stumpwm-user)

(set-prefix-key (kbd "C-i"))

(define-key *root-map* (kbd "m") 'mpd:*mpd-map*)
(define-key *root-map* (kbd "N") 'notifications:*notifications-map*)

(define-key *root-map* (kbd "c") "exec urxvt")
(define-key *root-map* (kbd "C-c") "exec urxvt")
(define-key *root-map* (kbd "C")
  "exec urxvt -fn -*-tamsyn-medium-r-*-*-40-*-*-*-*-*-*-* -e fish")

(define-key *root-map* (kbd "'") "windowlist")
(define-key *root-map* (kbd "\"") "pull-window-from-list")

(define-key *root-map* (kbd "n") "next-in-frame")
(define-key *root-map* (kbd "C-n") "next-in-frame")
(define-key *root-map* (kbd "p") "prev-in-frame")
(define-key *root-map* (kbd "C-p") "prev-in-frame")

(define-key stumpwm::*tile-group-top-map* (kbd "s-u") "echo tile")
(define-key stumpwm.floating-group::*float-group-top-map*
    (kbd "s-u") "echo float")

;;; Applications

(defprogram-shortcut emacs
  :key (kbd "s-e")
  :command "emacs"
  :props '(:class "Emacs")
  :pullp t
  :pull-key (kbd "s-E"))

(defprogram-shortcut firefox
  :key (kbd "s-f")
  :command "firefox -P default"
  :props '(:class "Firefox")
  :pullp t
  :pull-key (kbd "s-F"))

(defprogram-shortcut chromium
  :key (kbd "s-d")
  :command "chromium"
  :props '(:class "Chromium")
  :pullp t
  :pull-key (kbd "s-D"))

(defprogram-shortcut weechat
  :key (kbd "s-c")
  :command "urxvt -name irc -e weechat-curses"
  :props '(:instance "irc")
  :pullp t
  :pull-key (kbd "s-C"))

(defprogram-shortcut pms
  :key (kbd "s-m")
  :command "urxvt -name pms -e pms"
  :props '(:instance "pms"))

(defprogram-shortcut gimp
  :key (kbd "s-g")
  :command "gimp"
  :props '(:class "Gimp")
  :pullp t
  :pull-key (kbd "s-G"))

(defprogram-shortcut teamspeak
  :key (kbd "s-t")
  :command "teamspeak3"
  :props '(:class "TeamSpeak 3")
  :pullp t
  :pull-key (kbd "s-T"))

(defprogram-shortcut libreoffice
  :key (kbd "s-l")
  :command "libreoffice"
  :props '(:instance "libreoffice")
  :pullp t
  :pull-key (kbd "s-L"))

(defprogram-shortcut filezilla
  :key (kbd "s-z")
  :command "filezilla"
  :props '(:class "Filezilla")
  :pullp t
  :pull-key (kbd "s-Z"))

(defprogram-shortcut mpv
  :key (kbd "s-v")
  :command "mpv"
  :props '(:class "mpv")
  :pullp t
  :pull-key (kbd "s-V"))

(defprogram-shortcut telegram
  :key (kbd "s-p")
  :command "telegram"
  :props '(:class "Telegram")
  :pullp t
  :pull-key (kbd "s-P"))

(defprogram-shortcut zathura
  :key (kbd "s-a")
  :command "zathura"
  :props '(:class "Zathura")
  :pullp t
  :pull-key (kbd "s-A"))

;;; Miscellaneous

(defmacro defkeymap (name &body bindings)
  (let ((map-sym (gensym)))
    `(defparameter ,name
       (let ((,map-sym (make-sparse-keymap)))
         ,@(loop for (key command) in bindings
                 append
                 (if (listp key)
                     (loop for k in key
                           collect `(define-key ,map-sym (kbd ,k) ,command))
                     (list `(define-key ,map-sym (kbd ,key) ,command))))
         ,map-sym))))

;;; Dual screen configuration

(defcommand xrandr (args) ((:rest "xrandr "))
  (stumptray:stumptray)
  ;; Apparently we first have to close the tray, or the second monitor won't be
  ;; recognized
  (run-with-timer 1 nil #'run-shell-command (format nil "xrandr ~a" args))
  (run-with-timer 2 nil #'stumptray:stumptray))

(defkeymap *xrandr-map*
  ("v" "xrandr --output VGA1 --auto --right-of LVDS1")
  ("V" "xrandr --output VGA1 --auto --left-of LVDS1")
  ("C-v" "xrandr --output VGA1 --auto --same-as LVDS1")
  ("d" "xrandr --output HDMI1 --auto --right-of LVDS1")
  ("D" "xrandr --output HDMI1 --auto --left-of LVDS1")
  ("C-d" "xrandr --output HDMI1 --auto --same-as LVDS1")
  ("j" "xrandr --output DP-0 --auto --right-of HDMI-0")
  ("J" "xrandr --output DP-0 --auto --left-of HDMI-0")
  ("C-j" "xrandr --output DP-0 --auto --same-as HDMI-0")
  ("b" "exec sudo backlight 2000")
  ("B" "exec sudo backlight 900")
  ("C-b" "exec sudo backlight 300"))

(define-key *root-map* (kbd "d") '*xrandr-map*)

;;; Focus and window selection

(defkeymap *window-map*
  (("ESC" "C-g") "abort")
  (("n" "j") "move-focus down")
  (("p" "k") "move-focus up")
  (("b" "h") "move-focus left")
  (("f" "l") "move-focus right")
  (("N" "J") "move-window down")
  (("P" "K") "move-window up")
  (("B" "H") "move-window left")
  (("F" "L") "move-window right"))

(define-key *root-map* (kbd "w") '*window-map*)
