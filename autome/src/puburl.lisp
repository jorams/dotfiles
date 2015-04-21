(defpackage :autome/puburl
  (:use :cl :uiop :autome/util)
  (:export #:for
           #:cli))
(in-package :autome/puburl)

(defparameter *base-url* "http://files.joram.io/")

(defun for (file)
  (let ((file (merge-pathnames* (parse-unix-namestring file))))
    (optima:match (pathname-directory file)
      ((list* :absolute "home" "joram" "public"
              subpath)
       (let ((directory (mapcar #'quri:url-encode subpath))
             (name (when (pathname-name file)
                     (quri:url-encode (pathname-name file))))
             (type (when (pathname-type file)
                     (quri:url-encode (pathname-type file)))))
         (format nil "~a~{~a/~}~@[~a~@[.~a~]~]"
                 *base-url*
                 directory
                 name
                 type)))
      (otherwise (format *error-output* "File not public.~%")))))

(define-entry cli
    (:description "Retrieve public url for a file."
     :free free
     :commands (("<path>" "retrieve public url for <path>")))
  (let ((result (apply 'for free)))
    (prog1 result
      (when result
        (princ result)
        (terpri)))))
