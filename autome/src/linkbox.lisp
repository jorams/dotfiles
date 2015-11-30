(defpackage :autome/linkbox
  (:use :cl :uiop :autome/util
   :optima :optima.ppcre)
  (:import-from :alexandria
                #:plist-alist)
  (:export #:upload/shorten
           #:upload
           #:shorten
           #:cli))
(in-package :autome/linkbox)

(defparameter *url* "http://link.joram.io/")

(defun request (&rest parameters)
  (drakma:http-request
   *url*
   :method :post
   :parameters
   (append `(("auth" . ,(ubiquitous:value :linkbox :auth)))
           (plist-alist parameters))))

(defun shorten (url)
  (request "url" url))

(defun upload (path)
  (request "file" (parse-namestring path)))

(defun upload-stdin (type)
  (with-temporary-file (:stream stream
                        :pathname pathname
                        :element-type '(unsigned-byte 8)
                        :direction :output
                        :type (or type "tmp"))
    (slurp-input-stream stream
                        *standard-input*
                        :element-type '(unsigned-byte 8))
    (finish-output stream)
    (upload pathname)))

(defun upload/shorten (&optional path/url type)
  (match path/url
    ((and (or (ppcre "^mailto:")
              (ppcre "://"))
          url)
     (shorten url))
    (nil (upload-stdin type))
    (file (upload file))))

(define-entry cli
    (:description "Upload a file or create a short url."
     :free free
     :opts ((type #\t "File type, only relevant for STDIN" #'identity "TYPE"))
     :commands (("[path/url/]" "upload <path>, shorten <url> or post stdin")))
  (multiple-value-bind (result status)
      (upload/shorten (first free) type)
    (when (and result (= status 200))
      (prog1 result
        (princ result)
        (terpri)))))
