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
(define-key stumpwm::*float-group-top-map* (kbd "s-u") "echo float")

;;; Applications

(define-key *top-map* (kbd "s-e") "emacs")
(define-key *root-map* (kbd "e") "emacs")

(defprogram-shortcut firefox
  :key (kbd "s-f")
  :command "firefox -P default"
  :props '(:class "Firefox"))

(defprogram-shortcut chromium
  :key (kbd "s-d")
  :command "chromium"
  :props '(:class "Chromium"))

(defprogram-shortcut weechat
  :key (kbd "s-c")
  :command "urxvt -c irc -e weechat-curses"
  :props '(:instance "irc"))

(defprogram-shortcut pms
  :key (kbd "s-m")
  :command "urxvt -c pms -e pms"
  :props '(:instance "pms"))

(defprogram-shortcut gimp
  :key (kbd "s-g")
  :command "gimp"
  :props '(:class "Gimp"))

(defprogram-shortcut teamspeak
  :key (kbd "s-t")
  :command "teamspeak3"
  :props '(:class "TeamSpeak 3"))

(defprogram-shortcut libreoffice
  :key (kbd "s-l")
  :command "libreoffice"
  :props '(:instance "libreoffice"))

(defprogram-shortcut filezilla
  :key (kbd "s-z")
  :command "filezilla"
  :props '(:class "Filezilla"))

(defprogram-shortcut mysql-workbench
  :key (kbd "s-w")
  :command "mysql-workbench"
  :props '(:class "Mysql-workbench-bin"))

(defprogram-shortcut mpv
  :key (kbd "s-v")
  :command "mpv"
  :props '(:class "mpv"))

(defprogram-shortcut telegram
  :key (kbd "s-p")
  :command "telegram"
  :props '(:class "Telegram"))

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

(defkeymap *xrandr-map*
  ("v" "exec xrandr --output VGA1 --auto --right-of LVDS1")
  ("V" "exec xrandr --output VGA1 --auto --left-of LVDS1")
  ("d" "exec xrandr --output HDMI1 --auto --right-of LVDS1")
  ("D" "exec xrandr --output HDMI1 --auto --left-of LVDS1")
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
