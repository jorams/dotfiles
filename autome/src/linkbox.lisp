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

(defun upload/shorten (path/url)
  (match path/url
    ((and (or (ppcre "^mailto:")
              (ppcre "://"))
          url)
     (shorten url))
    (file (upload file))))

(define-entry cli
    (:description "Upload a file or create a short url."
     :free free
     :commands (("<path/url>" "upload <path> or shorten <url>")))
  (multiple-value-bind (result status)
      (apply 'upload/shorten free)
    (when (and result (= status 200))
      (prog1 result
        (princ result)
        (terpri)))))
