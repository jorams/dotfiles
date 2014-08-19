
(let ((circe-path
       (expand-file-name "site-lisp/circe/lisp" user-emacs-directory)))
  (add-to-list 'load-path circe-path))

(require 'circe)

(provide 'setup-circe)
