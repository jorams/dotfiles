(setq user-full-name "Joram Schrijver"
      user-mail-address "i@joram.io")

;; Configuration code is in .emacs.d/lisp
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir)
  ;; Custom settings in separate file
  (setq custom-file (expand-file-name "custom.el" lisp-dir))
  (load custom-file))

;; Add all vendor projects to load path
(let ((vendor-dir (expand-file-name "vendor" user-emacs-directory)))
  (add-to-list 'load-path vendor-dir)
  (dolist (project (directory-files vendor-dir t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

;;; Setup packages
(require 'setup-packages)

;;; Make it look pretty
(require 'appearance)

;;; General setup
(require 'defaults)
(require 'utils)
(require 'setup-ido)
(require 'setup-smex)
(require 'setup-slime)
(require 'setup-circe)
(require 'setup-helm)
(require 'setup-misc)
;;(require 'setup-evil)
(require 'setup-non-evil)
(require 'setup-mu4e)

;;; init.el ends here
