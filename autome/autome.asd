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
               #:optima.ppcre
               #:drakma
               #:cl-shellwords
               #:ubiquitous)
  :pathname "src"
  :components ((:file "util")
               (:file "volume")
               (:file "backlight")
               (:file "linkbox")
               (:file "defaults")
               (:file "browser")
               (:file "notiflier")
               (:file "dush")
               (:file "packup")))
