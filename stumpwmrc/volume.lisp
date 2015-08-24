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

(defun run-volume-command (vol &optional amount)
  (run-shell-command (case vol
                       (:up (format nil "pamixer --increase ~A" amount))
                       (:down (format nil "pamixer --decrease ~A" amount))
                       (:toggle "pamixer --toggle-mute")
                       (:muted "pamixer --get-mute")
                       (:update "pamixer --get-volume")
                       (t (format nil "pamixer --set-volume ~A" vol)))
                     t))

(defcommand volume (vol &optional (amount 5))
    ((:volume "Volume: ") :number)
  (handler-case
      (progn (run-volume-command vol amount)
             (setf *volume*
                   (if (ppcre:scan "true" (run-volume-command :muted))
                       "m"
                       (ppcre:scan-to-strings
                        "\\d+"
                        (run-volume-command :update)))))
    (serious-condition (c)
      (err "An error has occurred while managing the volume: ~S" c)))
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
