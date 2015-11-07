(defpackage :autome/browser
  (:use :cl :uiop :autome/util :optima :optima.ppcre)
  (:export #:cli))
(in-package :autome/browser)

(defun transform-url/file (url/file)
  (match url/file
    ((ppcre "https://www.irccloud.com/pastebin/([^/]+)" token)
     ;; The non-raw page tries to load boatloads of JS.
     (format nil "https://www.irccloud.com/pastebin/raw/~A" token))
    (url/file url/file)))

(define-entry cli
    (:description "Launch the default browser after optional URL-rewriting"
     :free free
     :commands (("<url/file>" "open <url/file> with the browser"))
     :opts ((dry-run #\d "Print the command that would be executed.")))
  (let ((url/file (apply #'concatenate 'string free)))
    (flet ((command (url/file)
             (format nil "firefox ~A"
                     (shellwords:escape (transform-url/file url/file)))))
      (if dry-run
          (format t "~A~%" (command url/file))
          (run! (command url/file))))))
