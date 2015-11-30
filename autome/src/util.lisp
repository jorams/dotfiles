(defpackage :autome/util
  (:use :cl :optima :optima.ppcre)
  (:export #:define-entry
           #:boolify
           #:boolify*
           #:run!))
(in-package :autome/util)

;;; Miscellaneous utilities

(defmacro run! (program)
  "Run program, return whether or not it succeeded."
  `(nth-value 2 (uiop:run-program ,program)))

(defun boolify (input)
  "Take an input (string) and test whether it is positive or negative.
Returns (values result match-found)"
  (match input
    ((ppcre "[yY].*?|on|en.*")
     (values t t))
    ((ppcre "[nN].*?|off|dis.*")
     (values nil t))
    (_ (values nil nil))))

(defun boolify* (input)
  "Like BOOLIFY, but returns either a list containing the result or NIL if no
valid value was found."
  (multiple-value-bind (result match-found)
      (boolify input)
    (when match-found (list result))))

;;; Program entry

(defun describe-commands (commands)
  "Used by DEFINE-ENTRY to output sub-command documentation."
  (with-output-to-string (out)
    (when commands
      (format out "~%Available commands:")
      (dolist (command commands)
        (if (consp command)
            (format out "~%  ~A~2,16@T~A"
                    (first command)
                    (second command))
            (format out "~% ~A" command))))))

;; TODO: Allow specifying a body per command?
;; TODO: More abstraction where possible
(defmacro define-entry (name (&key
                                description
                                opts
                                (free (gensym))
                                command
                                commands)
                        &body body)
  "Define a command line entry point function, with options sub-commands, etc.
For example:

  (define-entry consume-entry
      (:description \"Let's eat something!\"
       :free free-arguments
       :command command
       :commands ((\"eat\" \"just eat\")
                  (\"dine\" \"dine, fancily\"))
       :opts ((dish #\d \"the dish to consume\" #'identity \"DISH\")))
    (format t \"You have chosen to ~A the dish ~A\" command dish))

This entry point could then handle command line invocations like
 'consume eat -d steak'
 'consume dine --dish cake'
and output
 'You have chosen to eat the dish steak'
 'You have chosen to dine the dish cake'

The second one doesn't make any sense, but it's handled fine."
  (alexandria:with-gensyms (argv options)
    `(defun ,name (,argv)
       ;; Set up configuration
       (ubiquitous:restore :autome)
       ;; Let's go
       (opts:define-opts
           (:name :help
            :description "print this help text"
            :short #\h
            :long "help")
           ,@(loop for opt in opts
                   collect (destructuring-bind (sym short description
                                                &optional parser meta)
                               opt
                             (append
                              (list :name (alexandria:make-keyword sym)
                                    :short short
                                    :long (string-downcase (string sym))
                                    :description description)
                              (when parser (list :arg-parser parser))
                              (when meta (list :meta-var meta))))))
       (multiple-value-bind (,options ,free)
           (opts:get-opts ,argv)
         ;; Bind all options to their variables
         (destructuring-bind (&key ,@(mapcar #'first opts) &allow-other-keys)
             ,options
           (if (getf ,options :help)
               (prog1 t
                 (opts:describe
                  :prefix ,(format nil "~A~%~A"
                                   description
                                   (describe-commands commands))))
               (let (,@(when command
                         (list (list command
                                     `(when (find (first ,free)
                                                  ',(mapcar #'first commands)
                                                  :test #'string=)
                                        (pop ,free))))))
                 ,@body)))))))

