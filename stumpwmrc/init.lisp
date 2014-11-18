(in-package #:stumpwm-user)

(set-font '("-*-8x16 system font-*-*-*-*-*-*-*-*-*-*-*-*"
            "-*-tamsyn-*-r-*-*-20-*-*-*-*-*-*-*"))

#-j-modules-loaded
(progn
  (ql:quickload "swank")
  (set-contrib-dir "~/dev/lisp/stumpwm-contrib")
  (load-module "mpd")
  (load-module "notifications")
  (load-module "ttf-fonts"))

(handler-case
    (swank:create-server :dont-close t :port 4005)
  (serious-condition () (swank:create-server :dont-close t :port 4006)))

(set-font `("-*-8x16 system font-*-*-*-*-*-*-*-*-*-*-*-*"
            "-*-tamsyn-*-r-*-*-20-*-*-*-*-*-*-*"
            ,(make-instance 'xft:font :family "FontAwesome" :subfamily "Regular" :size 8)))

;;; Utility commands

(defcommand pull-window-from-list (&optional (fmt *window-format*)) (:rest)
  "Pull a window from anywhere in the current screen into the current frame."
  (let* ((windows (screen-windows (current-screen)))
         (window (stumpwm::select-window-from-menu windows fmt)))
    (if window
        (stumpwm::pull-window window)
        (throw 'error :abort))))

(defcommand shutdown () ()
  (run-shell-command "sudo systemctl poweroff"))

(defcommand reboot () ()
  (run-shell-command "sudo systemctl reboot"))

(defcommand-alias herscheep reboot)

;;; Key settings

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
  :command "st -c irc -e weechat-curses"
  :props '(:instance "irc"))

(defprogram-shortcut pms
  :key (kbd "s-m")
  :command "st -c pms -e pms"
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
  ("B" "exec sudo backlight 900"))

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

;;; Volume control and status

(define-stumpwm-type :volume (input prompt)
  (let* ((values '(("up" :up)
                   ("down" :down)
                   ("toggle" :toggle)
                   ("update" :update)))
         (volume (or (argument-pop input)
                     (completing-read (current-screen) prompt values)))
         (dir (or (second (assoc volume values :test 'string-equal))
                  (parse-integer volume :junk-allowed t))))
    (or dir (throw 'error "Invalid volume."))))

(defparameter *volume* "")

(defun volume-status (ml)
  (declare (ignore ml))
  *volume*)

(defun run-volume-command (vol amount)
  (run-shell-command (case vol
                       (:up (format nil "amixer set Master ~A%+" amount))
                       (:down (format nil "amixer set Master ~A%-" amount))
                       (:toggle "amixer set Master toggle")
                       (:update "amixer get Master")
                       (t (format nil "amixer set Master ~A%" vol)))
                     t))

(defcommand volume (vol &optional (amount 5))
    ((:volume "Volume: ") :number)
  (handler-case
       (let ((captures (nth-value 1 (ppcre:scan-to-strings
                                     "Mono: .*?\\[(:?([0-9]{1,3})%|off)\\]"
                                     (run-volume-command vol amount)))))
         (setf *volume* (or (elt captures 1) "m")))
    (serious-condition () (err "Master not found!")))
  (values))

(defkeymap *volume-map*
  ("u" "volume up")
  ("U" "volume up 10")
  ("d" "volume down")
  ("D" "volume down 10")
  ("V" "volume update")
  (("t" "v") "volume toggle"))

(define-key *root-map* (kbd "v") '*volume-map*)
(add-screen-mode-line-formatter #\V 'volume-status)

;; Update volume status for the first time.
(volume :update)

;;; Battery status

(defparameter *battery* "")

(defun battery-status (ml)
  (declare (ignore ml))
  *battery*)

(defcommand update-battery-status () ()
  (let* ((captures (nth-value 1 (ppcre:scan-to-strings
                                 "(Charging|Discharging|.),.*?(?:([0-9]{1,3})%)"
                                 (run-shell-command "acpi" t))))
         (status (or (parse-integer (elt captures 1) :junk-allowed t)
                     -1))
         (chargingp (string-equal "Charging" (elt captures 0))))
    (setf *battery*
          (format nil "^[~A~A^]"
                  (if chargingp "^(:fg 6)" "")
                  (cond ((< status 10) (format nil "^(:bg 1)~A" status))
                        ((< status 50) (format nil "^(:bg 3)~A" status))
                        (t (format nil "~A" status)))))))

(add-screen-mode-line-formatter #\B 'battery-status)
(run-with-timer 0 30 'update-battery-status)

;;; Selections

(define-stumpwm-type :selection (input prompt)
  (let* ((values '(("clipboard" :clipboard)
                   ("primary" :primary)
                   ("sync-clipboard" :sync-clipboard)
                   ("sync-primary" :sync-primary)
                   ("send-clipboard" :send-clipboard)
                   ("send-primary" :send-primary)))
         (raw-command (or (argument-pop input)
                          (completing-read (current-screen) prompt values)))
         (command (second (assoc raw-command values :test 'string-equal))))
    (or command (throw 'error "Invalid selection command."))))

(defcommand selection (command) ((:selection "Command: "))
  (case command
    (:clipboard (message (get-x-selection nil :clipboard)))
    (:primary (message (get-x-selection nil :primary)))
    (:send-clipboard (window-send-string (get-x-selection nil :clipboard)))
    (:send-primary (window-send-string (get-x-selection nil :primary)))
    (:sync-clipboard (set-x-selection (get-x-selection nil :clipboard) :primary))
    (:sync-primary (set-x-selection (get-x-selection nil :primary) :clipboard))))

(defkeymap *selection-map*
  ("c" "selection clipboard")
  ("C" "selection sync-clipboard")
  ("C-c" "selection send-clipboard")
  ("p" "selection primary")
  ("P" "selection sync-primary")
  ("C-p" "selection send-primary"))

(define-key *root-map* (kbd "t") '*selection-map*)


;;; Miscellaneous settings

(setf *input-window-gravity* :bottom)
(setf *message-window-gravity* :bottom)
(setf *window-border-style* :thin)
(set-focus-color "#222")

(setf *mouse-focus-policy* :sloppy)
(setf *new-frame-action* :empty)

(setf *startup-message*
      "^7*^BWelcome^b ^f1to the^f0 ^Bfuture^b.")

(setf stumpwm::*grab-pointer-character* 102)
(setf stumpwm::*grab-pointer-character-mask* 103)

(defmacro normalize-titles (&rest triplets)
  `(progn ,@(loop for (property string title) on triplets by #'cdddr
                  collect `(if (string= (,(case property
                                            (:class 'window-class)
                                            (:instance 'window-res))
                                         window) ,string)
                               (setf (window-user-title window) ,title)))))

(defun normalize-window-titles (window)
  (normalize-titles
   :class "Gimp" "gimp"
   :class "Firefox" "firefox"
   :instance "weechat" "weechat"))

(add-hook *new-window-hook* 'normalize-window-titles)

;;; Mode line

(notifications:notifications-add "^[^10day^]")

(defun mpd-status-icon ()
  (cond ((equal (mpd::assoc-value :state mpd::*mpd-status*) "play") "")
        ((equal (mpd::assoc-value :state mpd::*mpd-status*) "pause") "")
        ((equal (mpd::assoc-value :state mpd::*mpd-status*) "stop") "")))

(push '(#\i mpd-status-icon) mpd:*mpd-formatters-alist*)

(setf mpd:*mpd-modeline-fmt* "^f2%i^f0 %a - %A - %t (%n/%p)")

(setf *mode-line-position* :bottom)
(setf *screen-mode-line-format* "[^B%d v%V%% b%B%%^b] %W ^>%m(%N)")
(setf *time-modeline-string* "%H:%M")
(setf *mode-line-timeout* 10)
(setf *mode-line-background-color* "Black")
(setf *mode-line-border-color* "Black")
(setf *mode-line-highlight-template* "^B~A^b")

(defmethod stumpwm::group-add-head :after ((group stumpwm::tile-group) head)
  (toggle-mode-line (stumpwm::group-screen group) head))

(mode-line)

(defun hide-all-lower-windows (current last)
  (declare (ignore current last))
  (when (typep (current-group) 'stumpwm::tile-group)
    (mapc (lambda (win)
            (unless (eq win (stumpwm::frame-window
                             (stumpwm::window-frame win)))
              (stumpwm::hide-window win)))
          (group-windows (current-group)))))

(defcommand enable-hiding-lower-windows () ()
  "Enable a hook that hides all windows that aren't at the top of their frame.
This is primarily useful when you have (a) transparent window(s) and want to see
the wallpaper underneath instead of other windows."
  (add-hook *focus-window-hook* 'hide-all-lower-windows))

(defcommand firefox-url-as-vid () ()
  "Run Firefox's current url through 'vid', opening it in mpv.

This is incredibly ugly. It sends a bunch of keys to firefox to copy the url,
then pastes it into the command."
  (stumpwm::send-fake-key (current-window) (kbd "C-l"))
  (stumpwm::send-fake-key (current-window) (kbd "C-c"))
  (run-shell-command (format nil "vid '~a'" (get-x-selection))))

(define-key *top-map* (kbd "s-y") "firefox-url-as-vid")
