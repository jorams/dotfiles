(defpackage :autome/volume
  (:use :cl :uiop :autome/util :optima)
  (:shadow #:set)
  (:export #:up
           #:down
           #:set
           #:toggle
           #:status
           #:cli))
(in-package :autome/volume)

;;; Tooling

(defparameter +default-delta+ 5)
(defparameter +default-value+ 50)

(defun run-volume-command (command)
  (handler-case
      (ppcre:register-groups-bind (result enabled)
          ("Mono: .*?([0-9]{1,3})%.*?(on|off)"
           (run-program command :output :string))
        (if (string= enabled "on")
            result
            "m"))
    (serious-condition ()
      (format *error-output* "Mixer control Master not found!~%"))))

;;; API

(defun up (&optional (amount +default-delta+))
  (run-volume-command (format nil "amixer set Master ~A%+" amount)))

(defun down (&optional (amount +default-delta+))
  (run-volume-command (format nil "amixer set Master ~A%-" amount)))

(defun set (&optional (amount +default-value+))
  (run-volume-command (format nil "amixer set Master ~A%" amount)))

(defun toggle ()
  (run-volume-command "amixer set Master toggle"))

(defun status ()
  (run-volume-command "amixer get Master"))

;;; CLI

(define-entry cli
    (:description "Set or get audio volume"
     :free free
     :command command
     :commands (("up" "increase volume")
                ("down" "decrease volume")
                ("set" "set volume")
                ("<volume>" "set volume to <volume>")
                ("toggle" "toggle mute status")
                ("status" "get current volume (or 'm' when muted)")))
  (let ((result (match command
                  ("up" (apply 'up free))
                  ("down" (apply 'down free))
                  ("toggle" (toggle))
                  ("status" (status))
                  ;; The command is either "set" or something else
                  (_ (apply 'set free)))))
    (prog1 result
      (when result
        (princ result)
        (terpri)))))
