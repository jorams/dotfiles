(in-package #:stumpwm-user)

(defvar *soundboard-mixer* nil)
(defparameter *soundboard-directory* "~/.dump/soundboard/")
(defvar *soundboard-sounds* (make-hash-table :test #'equalp))
(defvar *soundboard-keymap* nil)

(defun start-mixer ()
  (setf *soundboard-mixer* (mixalot:create-mixer)))

(defun sound-file-type (path)
  (cond
    ((string-equal (pathname-type path) "mp3")
     :mp3)
    ((string-equal (pathname-type path) "ogg")
     :ogg)
    ((string-equal (pathname-type path) "flac")
     :flac)))

(defcommand index-sounds () ()
  "Index the *SOUNDBOARD-DIRECTORY* and refresh the available sounds."
  (let ((files (uiop:directory-files *soundboard-directory*))
        (keymap (make-sparse-keymap)))
    (define-key keymap (kbd "quoteleft") "clear-soundboard-mixer")
    (define-key keymap (kbd "~") "index-sounds")
    (clrhash *soundboard-sounds*)
    (mapc (lambda (file)
            (when (sound-file-type file)
              (ppcre:register-groups-bind (key name)
                  ("(?:([^-]+?)-)?(.*?)$" (pathname-name file))
                (setf (gethash name *soundboard-sounds*)
                      (cons (sound-file-type file)
                            (namestring file)))
                (when key
                  (define-key keymap (kbd key)
                    (format nil "soundboard ~A" name))))))
          files)
    (setf *soundboard-keymap* keymap)))

(defun start-soundboard ()
  (start-mixer)
  (index-sounds))

(defcommand clear-soundboard-mixer () ()
  "Stop all soundboard playback."
  (mixalot:mixer-remove-all-streamers *soundboard-mixer*))

(defcommand soundboard (sound)
    ((:string "Sound: "))
  "Play a sound!"
  (let ((sound (gethash sound *soundboard-sounds*)))
    (when sound
      (let ((streamer
              (case (car sound)
                (:mp3  (mixalot-mp3:make-mp3-streamer (cdr sound)))
                (:ogg  (mixalot-vorbis:make-vorbis-streamer (cdr sound)))
                (:flac (mixalot-flac:make-flac-streamer (cdr sound))))))
        (mixalot:mixer-add-streamer *soundboard-mixer* streamer)))))

(define-key *root-map* (kbd "z") '*soundboard-keymap*)

(start-soundboard)

;;; Superkeys -----------------------------------------------------------------

(defcommand superkey (&optional (key "SPC") (sound "space"))
    (:string :string)
  "Play a sound and send a key."
  (soundboard sound)
  (stumpwm::send-fake-key (current-window) (kbd key)))

(defcommand bind-superkey (&optional (key "SPC") (sound "space"))
    (:string :string)
  "Bind KEY in *TOP-MAP* to `SUPERKEY KEY SOUND'"
  (define-key *top-map* (kbd key) (format nil "superkey ~A ~A" key sound)))

(defcommand unbind-superkey (&optional (key "SPC"))
    (:string)
  "Unbind KEY in *TOP-MAP*, meant to undo the effect of BIND-SUPERKEY."
  (undefine-key *top-map* (kbd key)))
