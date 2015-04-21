(defpackage :autome/backlight
  (:use :cl :uiop :autome/util)
  (:shadow #:set)
  (:export #:up
           #:down
           #:set
           #:status
           #:cli))
(in-package :autome/backlight)

;;; Tooling

(defparameter +default-delta+ 100)
(defparameter +default-value+ 900)

(defparameter +backlight-file+
  #p"/sys/class/backlight/intel_backlight/brightness")

(defun status ()
  (parse-integer
   (read-file-string +backlight-file+)))

(defun set (&optional (value +default-value+))
  (with-open-file (stream +backlight-file+
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :error)
    (princ value stream)))

(defun up (&optional (amount +default-delta+))
  (set (+ (status) amount)))

(defun down (&optional (amount +default-delta+))
  (set (- (status) amount)))

(define-entry cli
    (:description "Set or get backlight brightness"
     :free free
     :command command
     :commands (("up" "increase brightness")
                ("down" "decrease brightness")
                ("set" "set brightness")
                ("<brightness>" "set brightness to <brightness>")
                ("status" "get current brightness")))
  (let ((result (cond
                  ((string= command "up")
                   (apply 'up (mapcar #'parse-integer free)))
                  ((string= command "down")
                   (apply 'down (mapcar #'parse-integer free)))
                  ((or (string= command "status")
                       (and (null command)
                            (null free)))
                   (status))
                  ;; The command is either "set" or something else
                  (t (apply 'set (mapcar #'parse-integer free))))))
    (prog1 result
      (when result
        (princ result)
        (terpri)))))
