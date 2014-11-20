(in-package #:stumpwm-user)

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
