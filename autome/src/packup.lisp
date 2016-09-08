(defpackage :autome/packup
  (:use :cl :uiop :autome/util)
  (:export #:cli))
(in-package :autome/packup)

(define-entry cli
    (:description "Compress some things into a .tar.xz file."
     :free free
     :opts ((file #\f "Output to NAME.tar.xz." #'identity "NAME"))
     :commands (("<file(s)>" "Compress <file(s)>")))
  (unless free
    (format *error-output* "No file(s) specified.~%"))
  (let* ((input-files (shellwords:join free))
         (output-file (format nil "~A.tar.xz"
                              (shellwords:escape (or file
                                                     (first free)))))
         (command (format nil "tar -cJf ~A ~A"
                          output-file
                          input-files)))
    (run! command :output :interactive
                  :error-output :interactive)))
