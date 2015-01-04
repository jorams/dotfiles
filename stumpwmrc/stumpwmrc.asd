(asdf:defsystem #:stumpwmrc
  :serial t
  :description "My StumpWM configuration"
  :author "Joram Schrijver <i@joram.io>"
  :depends-on (#:stumpwm #:swank #:mpd #:notifications #:ttf-fonts #:stumptray)
  :components ((:file "keys")
               (:file "selection")
               (:file "volume")
               (:file "util")
               (:file "battery")
               (:file "settings")))
