
(load "~/.private.el")
(let ((circe-path
       (expand-file-name "site-lisp/circe/lisp" user-emacs-directory)))
  (add-to-list 'load-path circe-path))

(require 'circe)
(require 'circe-color-nicks)

(setq circe-color-nicks-everywhere t)

(add-to-list 'ido-ignore-buffers "#.*")

(defun ido-switch-irc-buffer ()
  (interactive)
  (let ((ido-ignore-buffers '("^[^#]*$")))
    (ido-switch-buffer)))

(global-set-key (kbd "C-x I") 'ido-switch-irc-buffer)

(setq circe-network-options
      `(("zfn"
         :nick "joram"
         :host "irc.joram.io"
         :service 2345
         :pass ,(concat "joram/freenode:" znc-password))
        ("zfr"
         :nick "joram"
         :host "irc.joram.io"
         :service 2345
         :pass ,(concat "joram/freign:" znc-password))
        ("zim"
         :nick "joram"
         :host "irc.joram.io"
         :service 2345
         :pass ,(concat "joram/im:" znc-password))))

(provide 'setup-circe)
