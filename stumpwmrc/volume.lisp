(in-package #:stumpwm-user)

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
      (ppcre:register-groups-bind (result enabled)
          ("Mono: .*?([0-9]{1,3})%.*?(on|off)"
           (run-volume-command vol amount))
        (setf *volume* (if (string= enabled "on")
                           result
                           "m")))
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
