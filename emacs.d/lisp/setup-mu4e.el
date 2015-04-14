(require 'mu4e)

;;; Mu4e

(setq mu4e-maildir       "~/.mail"
      mu4e-sent-folder   "/fm/Sent Items"
      mu4e-drafts-folder "/fm/Drafts"
      mu4e-trash-folder  "/fm/Trash"
      mu4e-refile-folder "/fm/Archive"
      mu4e-get-mail-command "syncmail"
      mu4e-compose-signature "Joram"
      mu4e-change-filenames-when-moving t)

;;; Sending mail

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

;; Hotkey
(global-set-key (kbd "M-N") 'mu4e)

(provide 'setup-mu4e)
