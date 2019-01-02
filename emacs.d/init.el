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
(require 'diminish)

;;; Backups -------------------------------------------------------------------

(let ((backup-directory (expand-file-name
                         (concat user-emacs-directory "backups/"))))
  (setq backup-directory-alist
        `((".*" . ,backup-directory)))
  (setq auto-save-list-file-prefix
        (concat backup-directory "list-")))

;;; Visual tweaks -------------------------------------------------------------

;;; Disable the blinking cursor
(blink-cursor-mode -1)

;;; Highlight matching parentheses
(show-paren-mode 1)

;;; Whitespace mode
(setq-default whitespace-line-column nil)
(setq-default whitespace-style '(face
                                 trailing
                                 empty
                                 lines-tail
                                 indentation::space
                                 space-before-tab))
(global-whitespace-mode 1)

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
(setq-default fill-column 79)

;;; Handle CamelCase nicely
(global-subword-mode 1)
(diminish 'subword-mode)

;;; Delete selection before typing with region active
(delete-selection-mode)

;;; Sentences don't end with two spaces
(set-default 'sentence-end-double-space nil)

;;; Disable tab characters
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Automatically pair pairs
(electric-pair-mode 1)

;;; Make C-v and M-v scroll by half a page, maintaining the current screen
;;; position of the cursor.

(defun j/scroll-half-page-forward (&optional lines)
  (interactive "P")
  (let ((scroll-preserve-screen-position 1))
    (View-scroll-half-page-forward lines)))

(defun j/scroll-half-page-backward (&optional lines)
  (interactive "P")
  (let ((scroll-preserve-screen-position 1))
    (View-scroll-half-page-backward lines)))

(use-package view
  :config
  (bind-key "C-v" 'j/scroll-half-page-forward)
  (bind-key "M-v" 'j/scroll-half-page-backward))

;;; Remember point position
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;;; Start a server for emacsclients
(server-start)

;;; Allow multiple recursive minibuffers
(setq-default enable-recursive-minibuffers t)

;;; Enable some commands
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Always follow symlinks under version control
(setq vc-follow-symlinks t)

;;; Diminish various modes

(diminish 'abbrev-mode)
(diminish 'auto-fill-function "&")
(diminish 'visual-line-mode "\\")
(diminish 'eldoc-mode "doc")

;; Some modes are apparently hard to diminish
(defmacro really-diminish (mode replacement)
  (let ((function-name (intern (concat "j/diminish/" (symbol-name mode))))
        (hook-name (intern (concat (symbol-name mode) "-hook"))))
    `(progn
       (defun ,function-name ()
         (interactive)
         (diminish ',mode ,replacement))
       (add-hook ',hook-name ',function-name))))

(really-diminish org-indent-mode "»")
(really-diminish auto-revert-mode "#")

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

(defun j/linkbox (buffer start end type)
  (interactive
   (list (window-buffer (minibuffer-selected-window))
         (region-beginning)
         (region-end)
         (let ((name (buffer-file-name
                      (window-buffer
                       (minibuffer-selected-window)))))
           (if (or current-prefix-arg
                   (null name))
               (read-string "File extension: "
                            nil
                            nil
                            (and name (file-name-extension name)))
             (file-name-extension name)))))
  (with-temp-buffer
    (insert-buffer-substring buffer start end)
    (call-process-region (point-min)
                         (point-max)
                         shell-file-name
                         t
                         t
                         nil
                         shell-command-switch
                         (concat "linkbox --type " type))
    (kill-new (buffer-substring-no-properties (point-min)
                                              (point-max)))
    (message (buffer-substring-no-properties (point-min)
                                             (point-max)))))

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
  (load-theme 'doom-one t)
  (set-frame-font (font-spec :family "Input Mono"
                             :size (if (getenv "IS4K") 22 11))
                  nil
                  t))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (j/load-theme))))

(j/load-theme)

;;; Spaceline -----------------------------------------------------------------

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-default-separator 'slant
        powerline-height 25)
  (spaceline-emacs-theme)
  ;; These hooks cause the selected window to stay deselected when the frame
  ;; loses focus, until another action is performed. I don't particularly care
  ;; about their functionality, so I just remove them.
  (remove-hook 'focus-in-hook 'powerline-set-selected-window)
  (remove-hook 'focus-out-hook 'powerline-unset-selected-window))

;;; Undo Tree -----------------------------------------------------------------

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-history-directory-alist
        `((".*" . ,(expand-file-name
                    (concat user-emacs-directory "undo")))))
  :config
  (global-undo-tree-mode))

;;; Ivy -----------------------------------------------------------------------

;;; Small utilities

(defun j/run-urxvt-action (dir)
  (start-process "urxvt" nil
                 "urxvt" "-cd" dir))

(defun j/delete-file-action (file)
  (dired-delete-file file 'top t))

;;; Proper fuzzy matching and sorting
(use-package flx :ensure t)

(use-package counsel
  :ensure t
  :demand t
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume))
  :diminish counsel-mode
  :diminish ivy-mode
  :config
  ;; Enable keybinding overrides
  (ivy-mode 1)
  (counsel-mode 1)
  ;; Settings
  (setq ivy-use-virtual-buffers t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-re-builders-alist '((swiper . identity)
                                (t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil)
  ;; File navigation
  (bind-key "C-l" 'counsel-up-directory counsel-find-file-map)
  (ivy-add-actions
   'counsel-find-file
   '(("m" magit-status "magit")
     ("u" j/run-urxvt-action "urxvt")
     ("d" j/delete-file-action "delete"))))

;;; Avy -----------------------------------------------------------------------

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-r" . avy-goto-word-or-subword-1)
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

;;; Ediff ---------------------------------------------------------------------

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default ediff-split-window-function 'split-window-horizontally)

;;; Paredit -------------------------------------------------------------------

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :diminish "()"
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

;;; Highlight symbol ----------------------------------------------------------

(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol-mode)
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode))

;;; Whitespace cleanup --------------------------------------------------------

(use-package whitespace-cleanup-mode
  :ensure t
  :commands (global-whitespace-cleanup-mode)
  :diminish whitespace-cleanup-mode
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
         ("C-M->" . mc/mark-next-symbol-like-this)
         ("C-M-." . mc/unmark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/mark-previous-symbol-like-this)
         ("C-M-," . mc/unmark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)
         ("C-c M-i" . mc/insert-numbers)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (use-package mc-cycle-cursors))

;;; Shift-number --------------------------------------------------------------

(use-package shift-number
  :ensure t
  :bind (("C-M-+" . shift-number-up)
         ("C-M-_" . shift-number-down)))

;;; Aggressive indentation ----------------------------------------------------

(use-package aggressive-indent
  :ensure t
  :diminish "!"
  :config
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'c++-mode-hook #'aggressive-indent-mode)
  (add-hook 'lua-mode-hook #'aggressive-indent-mode))

;;; Slime-style elisp navigation ----------------------------------------------

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

;;; GNU Global tags -----------------------------------------------------------

(use-package ggtags
  :ensure t
  :diminish "gg"
  :config
  (add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'ggtags-mode)
  (add-hook 'php-mode-hook 'ggtags-mode))

;;; Company -------------------------------------------------------------------

(use-package company
  :ensure t
  :diminish company-mode
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
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

;;; Eldoc ---------------------------------------------------------------------

(use-package eldoc
  :ensure t
  :commands (turn-on-eldoc-mode))

;;; Org -----------------------------------------------------------------------

(use-package org-passwords
  :commands (org-passwords
             org-passwords-mode
             org-passwords-copy-password
             org-passwords-copy-username
             org-passwords-open-url
             org-passwords-random-words
             org-passwords-generate-password))

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
  :diminish yas-minor-mode
  :config (yas-global-mode))

;;; Magit ---------------------------------------------------------------------

(use-package magit
  :ensure t
  :commands (magit-status)
  :bind ("M-M" . magit-status)
  :config
  (setq git-commit-summary-max-length 50)
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (setq fill-column 72))))

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
  :ensure t
  :commands (ag
             ag-project
             ag-files
             ag-dired
             ag-dired-regexp)
  :bind ("C-c s s" . ag))

;;; Ripgrep -------------------------------------------------------------------

(use-package rg
  :ensure t
  :commands (rg-dwim
             rg-kill-saved-searches
             rg-list-searches
             rg-project
             rg
             rg-save-search
             rg-save-search-as-name
             rg-literal)
  :bind (("C-c s r" . rg)
         ("C-c p s r" . rg-project)))

;;; Projectile ----------------------------------------------------------------

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode))

;;; LSP -----------------------------------------------------------------------

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :config
  (push 'company-lsp company-backends))

;;; Editorconfig --------------------------------------------------------------

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; Lisp ----------------------------------------------------------------------

(use-package sly
  :ensure t
  :commands (sly sly-connect)
  :bind ("M-H" . hyperspec-lookup)
  :diminish ")"
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;;; Lua -----------------------------------------------------------------------

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq-default lua-indent-level 2))

(use-package company-lua
  :ensure t
  :commands company-lua
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq-local company-backends '(company-lua))
              (electric-indent-mode -1))))

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
         "\\.twig\\'"
         "\\.etlua\\'")
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook
            '(lambda ()
               (setq web-mode-enable-auto-pairing nil)
               (setq-local
                electric-pair-pairs
                (append electric-pair-pairs '((?% . ?%)))))))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode t))

(use-package company-web
  :ensure t
  :commands company-web-html
  :init
  (add-to-list 'company-backends 'company-web-html))

;;; PHP -----------------------------------------------------------------------

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package lsp-php
  :ensure t
  :after (php-mode lsp-mode)
  :config
  (add-hook 'php-mode-hook #'lsp-php-enable))

;;; Javascript ----------------------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

(use-package js2-refactor
  :ensure t
  :commands js2-refactor-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;;; CSS -----------------------------------------------------------------------

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package rainbow-mode
  :ensure t
  :diminish "🌈"
  :config
  (add-hook 'css-mode-hook 'rainbow-mode))
