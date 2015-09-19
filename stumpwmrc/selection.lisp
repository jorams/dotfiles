(in-package #:stumpwm-user)

(define-stumpwm-type :selection (input prompt)
  (let* ((values '(("clipboard"              :clipboard)
                   ("primary"                :primary)
                   ("sync-clipboard"         :sync-clipboard)
                   ("sync-primary"           :sync-primary)
                   ("send-clipboard"         :send-clipboard)
                   ("send-primary"           :send-primary)
                   ("send-clipboard-escaped" :send-clipboard-escaped)
                   ("send-primary-escaped"   :send-primary-escaped)))
         (raw-command (or (argument-pop input)
                          (completing-read (current-screen) prompt values)))
         (command (second (assoc raw-command values :test 'string-equal))))
    (or command (throw 'error "Invalid selection command."))))

(defcommand selection (command) ((:selection "Command: "))
  (case command
    (:clipboard
     (message (get-x-selection nil :clipboard)))
    (:primary
     (message (get-x-selection nil :primary)))
    (:sync-clipboard
     (set-x-selection (get-x-selection nil :clipboard) :primary))
    (:sync-primary
     (set-x-selection (get-x-selection nil :primary) :clipboard))
    (:send-clipboard
     (window-send-string (get-x-selection nil :clipboard)))
    (:send-primary
     (window-send-string (get-x-selection nil :primary)))
    (:send-clipboard-escaped
     (window-send-string (format nil "~S" (get-x-selection nil :clipboard))))
    (:send-primary-escaped
     (window-send-string (format nil "~S" (get-x-selection nil :primary))))))

(defkeymap *selection-map*
  ("c" "selection clipboard")
  ("C" "selection sync-clipboard")
  ("C-c" "selection send-clipboard")
  ("M-c" "selection send-clipboard-escaped")
  ("p" "selection primary")
  ("P" "selection sync-primary")
  ("C-p" "selection send-primary")
  ("M-p" "selection send-primary-escaped"))

(define-key *root-map* (kbd "t") '*selection-map*)
