(in-package :stumpwm-user)

;;; Preparation ---------------------------------------------------------------

(when (equalp (stumpwm:getenv "DOTFILE_TYPE")
              "laptop")
  (pushnew :system-has-battery *features*))

(when (stumpwm:getenv "IS4K")
  (pushnew :screen-is-4k *features*))

(ql:quickload '(#:slynk #:mpd #:notifications #:ttf-fonts #:stumptray))

;;; Remote control ------------------------------------------------------------

(defcommand slynk (&optional (port 4005)) (:number)
  (slynk:create-server :port port))

;;; Utilities -----------------------------------------------------------------

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

;;; Basic keybinds ------------------------------------------------------------

(set-prefix-key (kbd "C-i"))

(define-key *root-map* (kbd "m") 'mpd:*mpd-map*)
(define-key *top-map* (kbd "XF86AudioPlay") "mpd-toggle-pause")
(define-key *top-map* (kbd "XF86AudioPause") "mpd-toggle-pause")
(define-key *top-map* (kbd "XF86AudioNext") "mpd-next")
(define-key *top-map* (kbd "XF86AudioPrev") "mpd-prev")
(define-key *top-map* (kbd "s-F9") "mpd-toggle-pause")

(define-key *root-map* (kbd "N") 'notifications:*notifications-map*)

(define-key *root-map* (kbd "c") "exec urxvt")
(define-key *root-map* (kbd "C-c") "exec urxvt")
(define-key *root-map* (kbd "C")
  "exec urxvt -fn -*-tamsyn-medium-r-*-*-40-*-*-*-*-*-*-*")

(define-key *root-map* (kbd "'") "windowlist")
(define-key *root-map* (kbd "\"") "pull-window-from-list")

(define-key *root-map* (kbd "n") "next-in-frame")
(define-key *root-map* (kbd "C-n") "next-in-frame")
(define-key *root-map* (kbd "p") "prev-in-frame")
(define-key *root-map* (kbd "C-p") "prev-in-frame")

(define-key *root-map* (kbd "y") "exec xcolor | tr -d '\\n' | xsel --clipboard")

;;; Focus and window selection ------------------------------------------------

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

(define-key *top-map* (kbd "s-h")  "move-focus left")
(define-key *top-map* (kbd "s-j")  "move-focus down")
(define-key *top-map* (kbd "s-k")  "move-focus up")
(define-key *top-map* (kbd "s-l")  "move-focus right")
(define-key *top-map* (kbd "s-H")  "move-window left")
(define-key *top-map* (kbd "s-J")  "move-window down")
(define-key *top-map* (kbd "s-K")  "move-window up")
(define-key *top-map* (kbd "s-L")  "move-window right")

;;; Screen configuration ------------------------------------------------------

(defcommand xrandr (args) ((:rest "xrandr "))
  (stumptray:stumptray)
  ;; Apparently we first have to close the tray, or the second monitor won't be
  ;; recognized
  (run-with-timer 1 nil #'run-shell-command (format nil "xrandr ~a" args))
  (run-with-timer 2 nil #'stumptray:stumptray)
  (run-with-timer 2 nil 'enable-all-mode-lines)
  (run-with-timer 2 nil #'refresh-heads))

(defkeymap *xrandr-map*
  ;; erion
  ("j" "xrandr --output DP-0 --auto --right-of HDMI-0")
  ("J" "xrandr --output DP-0 --auto --left-of HDMI-0")
  ("C-j" "xrandr --output DP-0 --auto --same-as HDMI-0")
  ("k" "xrandr --output HDMI-0 --auto --right-of DP-0 --rotate right")
  ;; phyre
  ("n"
   "xrandr --output eDP-1 --mode 1920x1080 --output DP-2 --auto --right-of eDP-1")
  ("N"
   "xrandr --output eDP-1 --mode 1920x1080 --output DP-2 --auto --left-of eDP-1")
  ("C-n"
   "xrandr --output eDP-1 --mode 1920x1080 --output DP-2 --auto --same-as eDP-1")
  ;; Backlight, not all that xrandr-related
  ("b" "exec sudo backlight 2000")
  ("B" "exec sudo backlight 900")
  ("C-b" "exec sudo backlight 300")
  ;; Theme, also not xrandr-related
  ("y" "theme home")
  ("Y" "theme work")
  ;; Locking, similarly not xrandr-related
  ("l" "exec bash -c slock"))

(define-key *root-map* (kbd "d") '*xrandr-map*)

;;; Applications --------------------------------------------------------------

(defprogram-shortcut emacs
  :key (kbd "s-e")
  :command "emacs"
  :props '(:class "Emacs")
  :pullp t
  :pull-key (kbd "s-E"))

(defprogram-shortcut urxvt
  :key (kbd "s-x")
  :command "urxvt"
  :props '(:class "URxvt")
  :pullp t
  :pull-key (kbd "s-X"))

(defprogram-shortcut firefox
  :key (kbd "s-f")
  :command "systemd-run --user --slice firefox.slice --scope firefox -P default"
  :props '(:class "firefox")
  :pullp t
  :pull-key (kbd "s-F"))

(defprogram-shortcut chromium
  :key (kbd "s-d")
  :command "systemd-run --user --slice chromium.slice --scope chromium"
  :props '(:class "Chromium")
  :pullp t
  :pull-key (kbd "s-D"))

(defprogram-shortcut weechat
  :key (kbd "s-c")
  :command "urxvt -name irc -e weechat-curses"
  :props '(:instance "irc")
  :pullp t
  :pull-key (kbd "s-C"))

(defprogram-shortcut cantata
  :key (kbd "s-m")
  :command "cantata"
  :props '(:class "cantata")
  :pullp t
  :pull-key (kbd "s-M"))

(defprogram-shortcut gimp
  :key (kbd "s-g")
  :command "gimp"
  :props '(:class "Gimp")
  :pullp t
  :pull-key (kbd "s-G"))

(defprogram-shortcut inkscape
  :key (kbd "s-i")
  :command "inkscape"
  :props '(:class "Inkscape")
  :pullp t
  :pull-key (kbd "s-I"))

(defprogram-shortcut teamspeak
  :key (kbd "s-t")
  :command "teamspeak3"
  :props '(:class "TeamSpeak 3")
  :pullp t
  :pull-key (kbd "s-T"))

(defprogram-shortcut discord
  :key (kbd "s-r")
  :command "discord"
  :props '(:class "discord")
  :pullp t
  :pull-key (kbd "s-R"))

(defprogram-shortcut libreoffice
  :key (kbd "s-o")
  :command "libreoffice"
  :props '(:instance "libreoffice")
  :pullp t
  :pull-key (kbd "s-O"))

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
  :command "telegram-desktop"
  :props '(:class "TelegramDesktop")
  :pullp t
  :pull-key (kbd "s-P"))

(defprogram-shortcut zathura
  :key (kbd "s-a")
  :command "zathura"
  :props '(:class "Zathura")
  :pullp t
  :pull-key (kbd "s-A"))

(defprogram-shortcut android-studio
  :key (kbd "s-s")
  :command "android-studio"
  :props '(:class "jetbrains-studio")
  :pullp t
  :pull-key (kbd "s-S"))

(defprogram-shortcut insomnia
  :key (kbd "s-s")
  :command "insomnia"
  :props '(:class "Insomnia")
  :pullp t
  :pull-key (kbd "s-u"))

;;; Selection -----------------------------------------------------------------

(define-stumpwm-type :selection (input prompt)
  (let* ((values '(("clipboard"              :clipboard)
                   ("primary"                :primary)
                   ("sync-clipboard"         :sync-clipboard)
                   ("sync-primary"           :sync-primary)
                   ("send-clipboard"         :send-clipboard)
                   ("send-primary"           :send-primary)
                   ("send-clipboard-escaped" :send-clipboard-escaped)
                   ("send-primary-escaped"   :send-primary-escaped)))
         (raw-command (or (argument-pop input)
                          (completing-read (current-screen)
                                           prompt
                                           (mapcar 'first values))))
         (command (second (assoc raw-command values :test 'string-equal))))
    (or command (throw 'error "Invalid selection command."))))

(defcommand selection (command) ((:selection "Command: "))
  (case command
    (:clipboard
     (message (get-x-selection nil :clipboard)))
    (:primary
     (message (get-x-selection nil :primary)))
    (:sync-clipboard
     (set-x-selection (get-x-selection nil :clipboard) :primary))
    (:sync-primary
     (set-x-selection (get-x-selection nil :primary) :clipboard))
    (:send-clipboard
     (window-send-string (get-x-selection nil :clipboard)))
    (:send-primary
     (window-send-string (get-x-selection nil :primary)))
    (:send-clipboard-escaped
     (window-send-string (format nil "~S" (get-x-selection nil :clipboard))))
    (:send-primary-escaped
     (window-send-string (format nil "~S" (get-x-selection nil :primary))))))

(defkeymap *selection-map*
  ("c" "selection clipboard")
  ("C" "selection sync-clipboard")
  ("C-c" "selection send-clipboard")
  ("M-c" "selection send-clipboard-escaped")
  ("p" "selection primary")
  ("P" "selection sync-primary")
  ("C-p" "selection send-primary")
  ("M-p" "selection send-primary-escaped"))

(define-key *root-map* (kbd "t") '*selection-map*)

;;; Volume --------------------------------------------------------------------

;;; Status

(defparameter *volume* "")

(defun volume-status (ml)
  (declare (ignore ml))
  *volume*)

(defun volume-muted-p (&key sink source)
  (and
   (ppcre:scan "true"
               (run-shell-command
                (cond
                  ((eq sink t) (format nil "pamixer --get-mute"))
                  (sink (format nil "pamixer --sink ~A --get-mute" sink))
                  ((eq source t) (format nil "pamixer --default-source --get-mute"))
                  (source (format nil "pamixer --source ~A --get-mute" source))
                  (t (error "Neither a sink nor a source selected")))
                t))
   t))

(defun volume-percentage (&key sink source)
  (let* ((command (cond
                    ((eq sink t) (format nil "pamixer --get-volume"))
                    (sink (format nil "pamixer --sink ~A --get-volume" sink))
                    ((eq source t) (format nil "pamixer --default-source --get-volume"))
                    (source (format nil "pamixer --source ~A --get-volume" source))
                    (t (error "Neither a sink nor a source selected"))))
         (volume-string (ppcre:scan-to-strings
                         "\\d+"
                         (run-shell-command command t))))
    (or (parse-integer (or volume-string "") :junk-allowed t)
        -1)))

(defcommand update-volume-status () ()
  (let* ((default-muted-p (volume-muted-p :sink "default-out"))
         (default-volume (volume-percentage :sink "default-out"))
         (voice-muted-p (volume-muted-p :sink "voice-out"))
         (voice-volume (volume-percentage :sink "voice-out"))
         (mic-muted-p (volume-muted-p :source t))
         (mic-volume (volume-percentage :source t)))
    (setf *volume* (format nil "^[^f2 ^f0~a ^f2 ^f0~a ^f2 ^f0~a^]"
                           (if default-muted-p
                               ""
                               (format nil "~a%" default-volume))
                           (if voice-muted-p
                               ""
                               (format nil "~a%" voice-volume))
                           (if mic-muted-p
                               ""
                               (format nil "~a%" mic-volume))))))

(add-screen-mode-line-formatter #\V 'volume-status)

;;; Change

(define-stumpwm-type :volume-command (input prompt)
  "Either up/down/toggle, or the percentage to set it to."
  (let* ((values '(("up" :up)
                   ("down" :down)
                   ("toggle" :toggle)))
         (volume (or (argument-pop input)
                     (completing-read (current-screen)
                                      prompt
                                      (mapcar 'first values))))
         (dir (or (second (assoc volume values :test 'string-equal))
                  (parse-integer volume :junk-allowed t))))
    (or dir (throw 'error "Invalid volume."))))

(define-stumpwm-type :volume-target (input prompt)
  (let* ((values '("default-out" "voice-out" "mic"))
         (command (or (argument-pop input)
                      (completing-read (current-screen) prompt values))))
    (or command (throw 'error "Invalid volume command."))))

(defcommand volume (command target &optional (amount 5))
    ((:volume-command "Command: ")
     (:volume-target "Target: ")
     :number)
  (let ((pamixer-target (if (string-equal target "mic")
                            "--default-source"
                            (format nil "--sink ~A" target)))
        (pamixer-action (case command
                          (:up (format nil "--increase ~A" amount))
                          (:down (format nil "--decrease ~A" amount))
                          (:toggle (format nil "--toggle-mute"))
                          (t (format nil "--set-volume ~A" command)))))
    (run-shell-command (format nil "pamixer --allow-boost ~A ~A"
                               pamixer-target
                               pamixer-action)))
  (update-volume-status)
  (values))

(define-key *top-map* (kbd "s-F12") "volume up default-out")
(define-key *top-map* (kbd "s-S-F12") "volume up voice-out")
(define-key *top-map* (kbd "s-C-F12") "volume up mic")
(define-key *top-map* (kbd "s-F11") "volume down default-out")
(define-key *top-map* (kbd "s-S-F11") "volume down voice-out")
(define-key *top-map* (kbd "s-C-F11") "volume down mic")
(define-key *top-map* (kbd "s-F10") "volume toggle default-out")
(define-key *top-map* (kbd "s-S-F10") "volume toggle voice-out")
(define-key *top-map* (kbd "s-C-F10") "volume toggle mic")

;; Update volume status for the first time.
(update-volume-status)

;;; Quick screenshot or recording ---------------------------------------------

(defcommand screenshot-clipboard () ()
  (run-shell-command "screenshot -c"))

(defcommand screenshot-file (filename) ((:string "File name: "))
  (when filename
    (run-shell-command
     (format nil
             "screenshot -f '~a'"
             filename))))

(defcommand screenvid-start
    (filename include-audio-p)
    ((:string "File name: ") (:y-or-n "Include audio? "))
  (when filename
    (run-shell-command
     (format nil
             "systemd-run --user --scope --unit=screenvid screenvid -f '~a' ~a"
             filename
             (if include-audio-p "-a" "")))))

(defcommand screenvid-stop () ()
  (run-shell-command "systemctl --user stop screenvid.scope"))

(defkeymap *recording-map*
  ("c" "screenshot-clipboard")
  ("f" "screenshot-file")
  ("v" "screenvid-start")
  ("V" "screenvid-stop"))

(define-key *root-map* (kbd "V") '*recording-map*)

;;; Window titles -------------------------------------------------------------

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
   :class "Inkscape" "inkscape"
   :class "firefox" "firefox"
   :instance "irc" "irc"
   :instance "youtube-vid" "ytv"
   :class "Filezilla" "filezilla"
   :class "Chromium" "chromium"
   :class "Steam" "steam"
   :class "Insomnia" "insomnia"
   :instance "virt-manager" "virt-manager"
   :class "chatterino" "chatterino"
   :class "discord" "discord"))

(add-hook *new-window-hook* 'normalize-window-titles)

;;; Hiding non-focused windows ------------------------------------------------

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
This is primarily useful when you have (a) transparent window(s) and want to
see the wallpaper underneath instead of other windows."
  (add-hook *focus-window-hook* 'hide-all-lower-windows))

;;; Open Firefox URLs in mpv --------------------------------------------------

(defcommand firefox-url-as-vid () ()
  "Run Firefox's current url through 'vid', opening it in mpv.

This is incredibly ugly. It sends a bunch of keys to firefox to copy the url,
then pastes it into the command."
  (stumpwm::send-fake-key (current-window) (kbd "C-l"))
  (stumpwm::send-fake-key (current-window) (kbd "C-c"))
  (run-with-timer 0.5 nil (lambda ()
                            (run-shell-command
                             (format nil "mpv '~a'" (get-x-selection))))))

(define-key *top-map* (kbd "s-y") "firefox-url-as-vid")

;;; Window pulling ------------------------------------------------------------

(defcommand pull-window-from-list (&optional (fmt *window-format*)) (:rest)
  "Pull a window from anywhere in the current screen into the current frame."
  (let* ((windows (screen-windows (current-screen)))
         (window (stumpwm::select-window-from-menu windows fmt)))
    (if window
        (stumpwm::pull-window window)
        (throw 'error :abort))))

;;; Power control -------------------------------------------------------------

(defcommand shutdown () ()
  (run-shell-command "poweroff"))

(defcommand reboot () ()
  (run-shell-command "reboot"))

(defcommand-alias deboot shutdown)
(defcommand-alias poweroff shutdown)
(defcommand-alias ontscheep shutdown)
(defcommand-alias herscheep reboot)

;;; Appearance ----------------------------------------------------------------

(set-font `(#-screen-is-4k "-*-tamsyn-medium-r-*-*-12-*-*-*-*-*-*-*"
            #-screen-is-4k "-*-tamsyn-medium-r-*-*-9-*-*-*-*-*-*-*"
            #+screen-is-4k "-*-tamsyn-medium-r-*-*-20-*-*-*-*-*-*-*"
            #+screen-is-4k "-*-tamsyn-medium-r-*-*-16-*-*-*-*-*-*-*"
            #-screen-is-4k ,(make-instance 'xft:font
                                           :family "FontAwesome"
                                           :subfamily "Regular"
                                           :size 8)
            #+screen-is-4k ,(make-instance 'xft:font
                                           :family "FontAwesome"
                                           :subfamily "Regular"
                                           :size 12)))

(setf *input-window-gravity* :bottom)
(setf *message-window-gravity* :bottom)
(setf *window-border-style* :thin)
(setf *normal-border-width* 0)
(setf *maxsize-border-width* 0)

(setf *mouse-focus-policy* :click)
(setf *new-frame-action* :empty)
(setf *honor-window-moves* nil)

(setf *startup-message*
      "^7*^BWelcome^b ^f1to the^f0 ^Bfuture^b.")

(setf stumpwm::*grab-pointer-character* 102)
(setf stumpwm::*grab-pointer-character-mask* 103)

(define-stumpwm-type :theme (input prompt)
  (let* ((values '(("home" :home)
                   ("work" :work)))
         (raw-theme (or (argument-pop input)
                        (completing-read (current-screen)
                                         prompt
                                         (mapcar 'first values))))
         (theme (second (assoc raw-theme values :test 'string-equal))))
    (or theme (throw 'error "Invalid theme."))))

(defcommand theme (theme) ((:theme "Theme: "))
  (case theme
    (:home
     (set-focus-color "#222")
     (set-bg-color "#1c023c")
     (set-border-color "#1c023c")
     (set-win-bg-color "#1c023c")
     (setf *mode-line-background-color* "#1c023c")
     (setf *mode-line-border-color* "#1c023c"))
    (:work
     (set-focus-color "#222")
     (set-bg-color "#3b3b3a")
     (set-border-color "#ea7c21")
     (set-win-bg-color "#3b3b3a")
     (setf *mode-line-background-color* "#3b3b3a")
     (setf *mode-line-border-color* "#3b3b3a")))
  ;; Make sure all colors are applied to the mode line
  (loop for head in (screen-heads (current-screen))
        do (toggle-mode-line (current-screen) head)
           (toggle-mode-line (current-screen) head))
  (stumptray:stumptray)
  (stumptray:stumptray)
  (run-shell-command (format nil "theme ~A" (string-downcase theme))))

;;; Mode line battery ---------------------------------------------------------

#+system-has-battery
(progn
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
            (format nil "^[^f2~A ^f0~A%^]"
                    (cond (chargingp "")
                          ((<= 0 status 5) "")
                          ((<= 5 status 25) "")
                          ((<= 25 status 50) "")
                          ((<= 50 status 75) "")
                          ((<= 75 status 100) "")
                          (t ""))
                    status))))

  (add-screen-mode-line-formatter #\B 'battery-status)
  (run-with-timer 0 30 'update-battery-status))

;;; Mode line -----------------------------------------------------------------

(defun mpd-status-icon ()
  (cond ((equal (mpd::assoc-value :state mpd::*mpd-status*) "play") "")
        ((equal (mpd::assoc-value :state mpd::*mpd-status*) "pause") "")
        ((equal (mpd::assoc-value :state mpd::*mpd-status*) "stop") "")))

(pushnew '(#\i mpd-status-icon) mpd:*mpd-formatters-alist* :test #'equal)

(notifications:notifications-add "^[^10day^]")

(setf *mode-line-position* :top)
(setf mpd:*mpd-modeline-fmt* "^f2%i^f0 %t (%n/%p)")
(setf *screen-mode-line-format* (concatenate 'string
                                             "[^B%d %V"
                                             #+system-has-battery " %B"
                                             "^b] %W ^>%m(%N)%T"))
(setf *mode-line-highlight-template* "^B~A^b")
(setf *time-modeline-string* "%H:%M")
(setf *mode-line-timeout* 10)

(defcommand enable-all-mode-lines () ()
  "Enable all mode lines on the current screen."
  (loop for head in (screen-heads (current-screen))
        do (enable-mode-line (current-screen) head t)))

;; Enable the mode line on all current heads
(enable-all-mode-lines)

;; Enable Stumptray
(stumptray::stumptray)

(theme :home)

(run-shell-command "gpg-connect-agent updatestartuptty /bye >/dev/null")
