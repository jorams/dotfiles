(defpackage :autome/dush
  (:use :cl :uiop :autome/util)
  (:export #:cli))
(in-package :autome/dush)

(define-entry cli
    (:description "Display file sizes, sorted and human-readable."
     :free free
     :opts ((no-sort #\n "Don't sort the output by filesize.")
            (reverse #\r "Reverse sort to large->small."))
     :commands (("[file(s)]" "Display file size for [file(s)]")))
  (let* ((files (shellwords:join free))
         (sort-command (if reverse "sort -hr" "sort -h"))
         (command (if no-sort
                      (format nil "du -sh ~A" files)
                      (format nil "du -sh ~A | ~A"
                              files
                              sort-command))))
    (run! command :output :interactive
                  :error-output :interactive)))
