;;; Me ------------------------------------------------------------------------

(setq user-full-name "Joram Schrijver"
      user-mail-address "i@joram.io")

(load-file "~/.private.el")

;;; Package installation ------------------------------------------------------

;;; Initialize package.el
(package-initialize nil)

;;; Allow loading locally installed packages
(let ((vendor-dir (expand-file-name "vendor" user-emacs-directory)))
  (add-to-list 'load-path vendor-dir)
  (dolist (project (directory-files vendor-dir t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

;;; Load packages from MELPA
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;;; Use use-package to load packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)

;;; Backups -------------------------------------------------------------------

(let ((backup-directory (expand-file-name
                         (concat user-emacs-directory "backups/"))))
  (setq backup-directory-alist
        `((".*" . ,backup-directory)))
  (setq auto-save-list-file-prefix
        (concat backup-directory "list-")))

;;; Visual tweaks -------------------------------------------------------------

;;; Highlight matching parentheses
(show-paren-mode 1)

;;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; ...except in the minibuffer
(defun j/hide-whitespace ()
  (setq show-trailing-whitespace nil))
(add-hook 'minibuffer-inactive-mode-hook #'j/hide-whitespace)
(add-hook 'magit-popup-mode-hook #'j/hide-whitespace)

;;; Indicate empty lines at the end of a buffer
(setq-default indicate-empty-lines t)

;;; Make buffer names unique in a nicer way
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;;; Turn off mouse/graphical interface
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (scroll-bar-mode -1))

;;; Disable startup screen
(setq inhibit-startup-screen t)

;;; Simplify frame title
(setq frame-title-format
      '(multiple-frames
        "%b"
        ("" invocation-name)))

;;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;;; Basic behaviour -----------------------------------------------------------

;;; Sync selection and X clipboard
(setq x-select-enable-clipboard t)

;;; Allow editing of compressed files
(auto-compression-mode t)

;;; Line length 79
(setq-default fill-column 80)

;;; Handle CamelCase nicely
(global-subword-mode 1)

;;; Delete selection before typing with region active
(delete-selection-mode)

;;; Sentences don't end with two spaces
(set-default 'sentence-end-double-space nil)

;;; Disable tab characters
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Remember point position
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;;; Utilities -----------------------------------------------------------------

(defun j/wrap-hard ()
  (interactive)
  (auto-fill-mode 'toggle))

(defun j/wrap-soft ()
  (interactive)
  (visual-fill-column-mode 'toggle)
  (visual-line-mode 'toggle))

(defun j/day ()
  (interactive)
  (find-file (expand-file-name (concat "~/life/day/"
                                       (format-time-string "%Y-%m-%d.day"))))
  (auto-fill-mode 1))

(defun j/fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) fill-column)
      (insert-char char))))

(defun j/fill-to-end-- ()
  (interactive)
  (j/fill-to-end ?\-))

(bind-key "M-F" 'j/fill-to-end--)

;;; Smarter C-a.
;;; From http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defun j/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'j/smarter-move-beginning-of-line)

(defun j/align-on-single-quote ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\) '" nil 0 t))

;;; Theme ---------------------------------------------------------------------

(add-to-list 'custom-theme-load-path
			 (expand-file-name (concat user-emacs-directory
									   "themes")))

(defun j/load-theme ()
  (set-frame-parameter nil 'background-mode 'dark)
  (load-theme 'solarized t)
  (set-frame-font (font-spec :family "Input Mono")
                  nil
                  t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (j/load-theme))))
  (j/load-theme))

;;; Undo Tree -----------------------------------------------------------------

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist
        `((".*" . ,(expand-file-name
                    (concat user-emacs-directory "undo")))))
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode))

;;; Helm ----------------------------------------------------------------------

(defun helm-magit-status (candidate)
  (interactive)
  (magit-status (file-name-directory candidate)))

(use-package helm
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t)
    (helm-mode))
  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "<C-tab>" 'helm-select-action helm-map)
  (bind-key "C-k" 'helm-find-files-up-one-level helm-find-files-map)
  (bind-key "C-j" 'helm-execute-persistent-action helm-find-files-map)
  (bind-key "M-M" (lambda ()
                    (interactive)
                    (with-helm-alive-p
                      (helm-quit-and-execute-action 'helm-magit-status)))
            helm-find-files-map))

;;; Avy -----------------------------------------------------------------------

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-c l" . avy-goto-line)))

;;; Ace Window ----------------------------------------------------------------

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind ("C-x o" . ace-window))

;;; Winner Mode ---------------------------------------------------------------

(winner-mode 1)

;;; Windmove ------------------------------------------------------------------

(bind-key "C-S-h" 'windmove-left)
(bind-key "C-S-j" 'windmove-down)
(bind-key "C-S-k" 'windmove-up)
(bind-key "C-S-l" 'windmove-right)

;;; Helm-swoop ----------------------------------------------------------------

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop helm-swoop-from-isearch)
  :init (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map))

;;; Autopair ------------------------------------------------------------------

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode))

;;; Paredit -------------------------------------------------------------------

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook                  'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook                        'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            'enable-paredit-mode)
  (add-hook 'scheme-mode-hook                      'enable-paredit-mode)
  :config
  (setq paredit-space-for-delimiter-predicates
        '((lambda (endp delimiter) nil))))

;;; Highlight parentheses -----------------------------------------------------

(use-package highlight-parentheses
  :ensure t
  :commands (highlight-parentheses-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

;;; Highlight symbol ----------------------------------------------------------

(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode))

;;; Whitespace cleanup --------------------------------------------------------

(use-package whitespace-cleanup-mode
  :ensure t
  :commands (global-whitespace-cleanup-mode)
  :init
  (global-whitespace-cleanup-mode))

(use-package visual-fill-column
  :ensure t
  :commands (visual-fill-column-mode))

;;; Expand region -------------------------------------------------------------

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;; Multiple cursors ----------------------------------------------------------

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)
         ("C-c M-i" . mc/insert-numbers)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;;; Aggressive indentation ----------------------------------------------------

(use-package aggressive-indent :ensure t :config
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'c++-mode-hook #'aggressive-indent-mode))

;;; Slime-style elisp navigation ----------------------------------------------

(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

;;; Company -------------------------------------------------------------------

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (unbind-key "RET" company-active-map)
  ;; This next line would normally use unbind-key, but that breaks on vectors.
  (bind-key [return] nil company-active-map)
  (bind-key "M-RET" 'company-complete-selection company-active-map)
  (unbind-key "TAB" company-active-map)
  ;; This next line would normally use unbind-key, but that breaks on vectors.
  (bind-key [tab] nil company-active-map)
  (bind-key "M-TAB" 'company-complete-common company-active-map))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

;;; Flycheck ------------------------------------------------------------------

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;;; Eldoc ---------------------------------------------------------------------

(use-package eldoc
  :ensure t
  :commands (turn-on-eldoc-mode))

;;; Org -----------------------------------------------------------------------

(use-package org-passwords)

(defvar j/org-notes-file "~/life/notes.org")
(bind-key "M-C" 'org-capture)

(setq
 org-capture-templates
 `(("p" "password"
    entry (file ,j/org-passwords-file)
    "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p")
   ("b" "Bookmark"
    entry (file+headline ,j/org-notes-file "Bookmarks")
    "* [[%^{URL}][%^{Title}]] %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?")
   ))

;;; Mail ----------------------------------------------------------------------

(use-package mu4e
  :config
  (setq mu4e-maildir       "~/.mail"
        mu4e-sent-folder   "/fm/Sent Items"
        mu4e-drafts-folder "/fm/Drafts"
        mu4e-trash-folder  "/fm/Trash"
        mu4e-refile-folder "/fm/Archive"
        mu4e-get-mail-command "syncmail"
        mu4e-compose-signature "Joram"
        mu4e-change-filenames-when-moving t)
  :bind (("M-N" . mu4e)))

(use-package smtpmail
  :config
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-smtp-server "mail.messagingengine.com"
        smtpmail-smtp-service 587))

;;; Yasnippet -----------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

;;; Magit ---------------------------------------------------------------------

(use-package magit
  :ensure t
  :commands (magit-status)
  :bind ("M-M" . magit-status))

;;; Git Time Machine ----------------------------------------------------------

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine))

;;; Transmission --------------------------------------------------------------

(use-package transmission
  :ensure t
  :commands (transmission)
  :config
  (bind-key "A" (lambda ()
                  (interactive)
                  (transmission-add (read-string "URI: ")))
            transmission-mode-map))

;;; The Silver Searcher--------------------------------------------------------

(use-package ag
  :ensure t)

;;; Projectile ----------------------------------------------------------------

(use-package projectile
  :ensure t
  :config (projectile-global-mode))

;;; Lisp ----------------------------------------------------------------------

(use-package sly
  :ensure t
  :config
  (setq common-lisp-hyperspec-root "file:///home/joram/.dump/HyperSpec/")
  :bind (("M-H" . common-lisp-hyperspec)))

(use-package sly-company
  :ensure t
  :config
  (setq sly-company-completion 'fuzzy)
  (defadvice sly-company-doc-buffer (around package-guard (candidate) activate)
    (unless (string-match-p ":$" candidate)
      ad-do-it))
  (add-hook 'sly-mode-hook 'sly-company-mode))

;;; Lua -----------------------------------------------------------------------

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package company-lua
  :ensure t
  :init
  (add-to-list 'company-backends 'company-lua))

;;; Markdown ------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.md\\'"))

;;; Web -----------------------------------------------------------------------

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.twig\\'")
  :config
  (setq-default web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode t))

;;; PHP -----------------------------------------------------------------------

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

;;; Javascript ----------------------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package js2-refactor
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'js2-mode-hook #'js2-refactor-mode t))

;;; CSS -----------------------------------------------------------------------

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

;;; Python --------------------------------------------------------------------

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode t)
  (add-hook 'python-mode-hook 'turn-on-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'company-mode t))

;;; C++ -----------------------------------------------------------------------

(defun enable-irony-mode ()
  "Only enable irony when actually in a supported major mode.
PHP-mode also extends c-mode, which normally triggers a warning."
  (when (member major-mode irony-supported-major-modes)
    (irony-mode 1)))

(use-package irony
  :ensure t
  :commands (irony-mode)
  :config
  (add-hook 'c++-mode-hook 'enable-irony-mode t)
  (add-hook 'c-mode-hook 'enable-irony-mode t)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc t))

(use-package company-irony
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :ensure t
  :config
  (flycheck-irony-setup))
