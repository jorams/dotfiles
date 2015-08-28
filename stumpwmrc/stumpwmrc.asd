(asdf:defsystem #:stumpwmrc
  :serial t
  :description "My StumpWM configuration"
  :author "Joram Schrijver <i@joram.io>"
  :depends-on (#:stumpwm #:swank #:mpd #:notifications #:ttf-fonts #:stumptray
                         #:mixalot #:mixalot-mp3 #:mixalot-vorbis #:mixalot-flac)
  :components ((:file "keys")
               (:file "selection")
               (:file "volume")
               (:file "util")
               #+system-has-battery (:file "battery")
               (:file "soundboard")
               (:file "settings")))
