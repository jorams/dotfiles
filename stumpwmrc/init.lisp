(in-package #:stumpwm-user)

(when (equalp (stumpwm:getenv "DOTFILE_TYPE")
              "laptop")
  (pushnew :system-has-battery *features*))

(asdf:load-system :stumpwmrc)
