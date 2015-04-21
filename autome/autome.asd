(in-package :asdf-user)
(asdf:defsystem :autome
  :description "Little utility scripts."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:unix-opts
               #:quri
               #:optima)
  :pathname "src"
  :components ((:file "util")
               (:file "volume")
               (:file "backlight")
               (:file "puburl")))
