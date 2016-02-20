(defpackage :autome/notiflier
  (:use :cl :uiop :autome/util)
  (:import-from :alexandria
                #:plist-alist)
  (:export #:send
           #:cli))
(in-package :autome/notiflier)

(defparameter *url* "https://notify.joram.io/send")

(defun request (&rest parameters)
  (drakma:http-request
   *url*
   :method :post
   :parameters
   (append `(("token" . ,(ubiquitous:value :notiflier :token)))
           (plist-alist parameters))))

(defun send (title &key target body importantp)
  (apply 'request (append (when target   `("to" ,target))
                          (when title      `("title" ,title))
                          (when body       `("message" ,body))
                          (when importantp `("important" nil)))))

(define-entry cli
    (:description "Notify."
     :free free
     :opts ((target #\t "Target receiver" #'identity "TARGET")
            (important #\i "Important"))
     :commands (("<title> [body]" "Send a notification")))
  (multiple-value-bind (result status)
      (send (first free)
            :target target
            :body (second free)
            :importantp important)
    (when (and result (= status 200))
      (prog1 result
        (princ result)
        (terpri)))))
