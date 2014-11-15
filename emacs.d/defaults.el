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

;;; Nicer scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)

;;; I don't double-space
(set-default 'sentence-end-double-space nil)

;;; Nicer buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; No tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Show excess whitespace
(setq-default show-trailing-whitespace t)

(defun minibuffer-hide-whitespace ()
  (setq show-trailing-whitespace nil))
(add-hook 'minibuffer-inactive-mode-hook #'minibuffer-hide-whitespace)

(setq-default indicate-empty-lines t)

;;; Dired
(setq dired-listing-switches "-al --group-directories-first")

(provide 'defaults)
