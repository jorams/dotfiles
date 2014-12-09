;;; Defaults --- Set some better defaults

;;; Turn off mouse/graphical interface
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Simplify frame title
(setq frame-title-format
      '(multiple-frames
        "%b"
        ("" invocation-name)))

;;; Selection to X clipboard
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;;; Allow editing of compressed files
(auto-compression-mode t)

;;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;;; Line length 80
(setq-default fill-column 80)

;;; Handle CamelCase nicely
(global-subword-mode 1)

;;; I don't double-space
(set-default 'sentence-end-double-space nil)

;;; No tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Dired
(setq dired-listing-switches "-al --group-directories-first")

;; Don't pollute the filesystem with all kinds of files
(let ((backup-directory (expand-file-name
                         (concat user-emacs-directory "backups/"))))
  (setq backup-directory-alist
        `((".*" . ,backup-directory)))
  (setq auto-save-list-file-prefix
        (concat backup-directory "list-")))

;; Persistent undo, in central directory
(setq undo-tree-history-directory-alist
      `((".*" . ,(expand-file-name
                  (concat user-emacs-directory "undo")))))
(setq undo-tree-auto-save-history t)

;;; Remember point position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;; Misc
(setq epg-gpg-program "/usr/bin/gpg")
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'defaults)

;;; defaults.el ends here
