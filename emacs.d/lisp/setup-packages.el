(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
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
     whitespace-cleanup-mode
     flycheck
     web-mode
     csharp-mode
     htmlize
     projectile
     anaconda-mode
     company-anaconda
     highlight-parentheses
     highlight-symbol
     js2-mode
     js2-refactor
     expand-region
     ace-jump-mode
     switch-window
     visual-fill-column
     writeroom-mode
     centered-cursor-mode
     magit
     transmission
     slime-company
     company-quickhelp
     yasnippet)))

;;; Now really
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'setup-packages)
