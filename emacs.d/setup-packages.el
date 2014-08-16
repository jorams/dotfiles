(require 'package)

(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;; Utility for installing multiple packages
(defun packages-install (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package)))
  (delete-other-windows))

;;; Load packages
(defun init--install-packages ()
  (packages-install
   '(evil
     company
     paredit
     evil-paredit
     elisp-slime-nav
     flx
     flx-ido
     whitespace-cleanup-mode
     ido-ubiquitous
     ido-vertical-mode
     ido-at-point
     smex
     flycheck
     web-mode
     htmlize)))

;;; Now really
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'setup-packages)
