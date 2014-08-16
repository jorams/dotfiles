(setq user-full-name "Joram Schrijver"
      user-mail-address "i@joram.io")

;;; Turn off mouse/graphical interface
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Disable startup screen
(setq inhibit-startup-screen t)

(add-to-list 'load-path user-emacs-directory)

;; Add all site-lisp projects to load path
(let ((site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path site-lisp-dir)
  (dolist (project (directory-files site-lisp-dir t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

;;; Custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(let ((backup-directory (expand-file-name
                         (concat user-emacs-directory "backups/"))))
  (setq backup-directory-alist
        `((".*" . ,backup-directory)))
  ;; (setq auto-save-file-name-transforms
  ;;      `((".*" ,backup-directory t)))
  (setq auto-save-list-file-prefix
        (concat backup-directory "list-")))

;;; Remember point position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;; Setup packages
(require 'setup-packages)

;;; Keep persistent undo in different directory
(setq undo-tree-history-directory-alist
      `((".*" . ,(expand-file-name
                  (concat user-emacs-directory "undo")))))

;;; Persistent undo
(setq undo-tree-auto-save-history t)

;;; Make it look pretty
(require 'appearance)

;;; Some packages
(require 'multiple-cursors)
(require 'fill-column-indicator)
(require 'magit)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Some keybindings
;; (global-set-key (kbd "C-;") (lookup-key (current-global-map) (kbd "C-x")))

(setq epg-gpg-program "/usr/bin/gpg")

;;; General setup
(require 'defaults)
(require 'utils)
(require 'setup-ido)
(require 'setup-smex)
(require 'setup-slime)
(require 'setup-paredit)
(require 'setup-markdown)
(require 'setup-web-mode)
(require 'setup-lua)
(require 'setup-arduino)
(require 'setup-less)
(require 'setup-evil)
