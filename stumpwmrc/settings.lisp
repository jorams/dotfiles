(in-package #:stumpwm-user)

;;; Enable connection using SLIME

(handler-case
    (swank:create-server :dont-close t :port 4005)
  (serious-condition () (swank:create-server :dont-close t :port 4006)))

;;; Miscellaneous settings

(reset-font)

(setf *input-window-gravity* :bottom)
(setf *message-window-gravity* :bottom)
(setf *window-border-style* :thin)
(setf *normal-border-width* 0)
(setf *maxsize-border-width* 0)
(set-focus-color "#222")

(setf *mouse-focus-policy* :sloppy)
(setf *new-frame-action* :empty)

(setf *startup-message*
      "^7*^BWelcome^b ^f1to the^f0 ^Bfuture^b.")

(setf stumpwm::*grab-pointer-character* 102)
(setf stumpwm::*grab-pointer-character-mask* 103)

;;; Mode line

(notifications:notifications-add "^[^10day^]")

(setf *mode-line-position* :bottom)
(setf mpd:*mpd-modeline-fmt* "^f2%i^f0 %a - %A - %t (%n/%p)")
(setf *screen-mode-line-format* "[^B%d v%V%% b%B%%^b] %W ^>%m(%N)")
(setf *time-modeline-string* "%H:%M")
(setf *mode-line-timeout* 10)
(setf *mode-line-background-color* "Black")
(setf *mode-line-border-color* "Black")
(setf *mode-line-highlight-template* "^B~A^b")

;; Automatically enable the mode line on new heads
(defmethod stumpwm::group-add-head :after ((group stumpwm::tile-group) head)
  (toggle-mode-line (stumpwm::group-screen group) head))

;; Enable the mode line on the first head
(mode-line)

;; Enable Stumptray
(stumptray::stumptray)
