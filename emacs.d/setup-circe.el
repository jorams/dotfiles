
(load "~/.private.el")
(let ((circe-path
       (expand-file-name "site-lisp/circe/lisp" user-emacs-directory)))
  (add-to-list 'load-path circe-path))

(require 'circe)
(require 'circe-color-nicks)

(setq circe-color-nicks-everywhere t)
(enable-circe-color-nicks)

(add-to-list 'ido-ignore-buffers "#.*")
(add-to-list 'ido-ignore-buffers "irc\.joram\.io:.*")

(defun ido-switch-irc-buffer ()
  (interactive)
  (let ((ido-ignore-buffers '("^[^#]*$")))
    (ido-switch-buffer)))

(defun irc-frame ()
  (interactive)
  (make-frame '((name . "irc"))))

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
