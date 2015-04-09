(in-package #:stumpwm-user)


;;; Normalize window titles

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
   :instance "irc" "irc"
   :instance "youtube-vid" "ytv"
   :class "Filezilla" "filezilla"
   :class "Chromium" "chromium"))

(add-hook *new-window-hook* 'normalize-window-titles)


;;; Hiding all non-focused windows.

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


;;; Open firefox urls in mpv (through youtube-dl)

(defcommand firefox-url-as-vid () ()
  "Run Firefox's current url through 'vid', opening it in mpv.

This is incredibly ugly. It sends a bunch of keys to firefox to copy the url,
then pastes it into the command."
  (stumpwm::send-fake-key (current-window) (kbd "C-l"))
  (stumpwm::send-fake-key (current-window) (kbd "C-c"))
  (run-shell-command (format nil "vid '~a'" (get-x-selection))))

(define-key *top-map* (kbd "s-y") "firefox-url-as-vid")

;;; Pulling windows into the frame from a list

(defcommand pull-window-from-list (&optional (fmt *window-format*)) (:rest)
  "Pull a window from anywhere in the current screen into the current frame."
  (let* ((windows (screen-windows (current-screen)))
         (window (stumpwm::select-window-from-menu windows fmt)))
    (if window
        (stumpwm::pull-window window)
        (throw 'error :abort))))


;;; Power control

(defcommand shutdown () ()
  (run-shell-command "sudo systemctl poweroff"))

(defcommand reboot () ()
  (run-shell-command "sudo systemctl reboot"))

(defcommand-alias deboot shutdown)
(defcommand-alias ontscheep shutdown)
(defcommand-alias herscheep reboot)


;;; MPD play/pause/stop icon for in the mode line

(defun mpd-status-icon ()
  (cond ((equal (mpd::assoc-value :state mpd::*mpd-status*) "play") "")
        ((equal (mpd::assoc-value :state mpd::*mpd-status*) "pause") "")
        ((equal (mpd::assoc-value :state mpd::*mpd-status*) "stop") "")))


(push '(#\i mpd-status-icon) mpd:*mpd-formatters-alist*)


;;; Font settings

(defun reset-font ()
  (set-font `(,(make-instance 'xft:font
                              :family "DejaVu Sans Mono"
                              :subfamily "Book"
                              :size 12)
              "-*-tamsyn-*-r-*-*-20-*-*-*-*-*-*-*"
              ,(make-instance 'xft:font
                              :family "FontAwesome"
                              :subfamily "Regular"
                              :size 8))))
