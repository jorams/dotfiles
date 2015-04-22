(defpackage :autome/defaults
  (:use :cl :uiop :autome/util :optima :optima.ppcre)
  (:export #:keyboard
           #:beep
           #:dpms
           #:fonts
           #:cli))
(in-package :autome/defaults)

(defun keyboard (&optional (caps :control))
  "Properly set the keyboard mapping."
  (check-type caps (member :control :escape))
  (zerop (nth-value 2 (run-program
                       (list "setxkbmap"
                             "-option" ""
                             "-option" "compose:menu"
                             "-option" (ecase caps
                                         (:control "ctrl:nocaps")
                                         (:escape "caps:escape")))))))

(defun beep (&optional enablep)
  "Control whether or not the system beeps."
  (zerop (nth-value 2 (run-program (list "xset" (if enablep "+b" "-b"))))))

(defun dpms (&optional (enablep t))
  "Control whether or not the screen turns off after a while."
  (zerop (nth-value 2
                    (if enablep
                        (run-program "xset +dpms dpms 0 0 300 s on")
                        (run-program "xset -dpms s off")))))

(defun fonts ()
  (zerop (nth-value 2
                    (run-program "xset +fp /usr/share/fonts/local fp rehash"))))

(define-entry cli
    (:description "Control miscellaneous settings"
     :free free
     :command command
     :commands (("keyboard" "set keyboard mapping")
                ("beep" "enable/disable system beep")
                ("dpms" "enable/disable screen power saving")
                ("fonts" "reload X fonts")
                ("all" "set all defaults")))
  (handler-case
      (match command
        ("beep"
         (apply 'beep (boolify* (first free))))
        ("dpms"
         (apply 'dpms (boolify* (first free))))
        ("keyboard"
         (match (first free)
           ((ppcre "^[eE].*")
            (keyboard :escape))
           ((ppcre "^[cC].*")
            (keyboard :control))
           (_ (keyboard))))
        ("fonts" (fonts))
        ;; The command is either "all" or something else
        (_ (reduce (lambda (x y) (and x y))
                   (list (keyboard)
                         (beep)
                         (dpms)))))
    (serious-condition () nil)))
