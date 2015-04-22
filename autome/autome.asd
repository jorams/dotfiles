(in-package :asdf-user)
(asdf:defsystem :autome
  :description "Little utility scripts."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:unix-opts
               #:quri
               #:optima
               #:optima.ppcre)
  :pathname "src"
  :components ((:file "util")
               (:file "volume")
               (:file "backlight")
               (:file "puburl")
               (:file "defaults")))
