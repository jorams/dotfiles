;; -*- lexical-binding: t; -*-
;;; Speed up init -------------------------------------------------------------

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar j//file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)
            (setq file-name-handler-alist j//file-name-handler-alist)))

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
  (dolist (project (directory-files vendor-dir t "\\w+" t))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

;;; Load packages from MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

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
        `(("." . ,backup-directory)))
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
                                 lines-tail
                                 indentation::space
                                 space-before-tab))
(setq-default whitespace-global-modes '(not magit-status-mode magit-log-mode))
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

;;; Improve M-SPC
(bind-key "M-SPC" 'cycle-spacing)

;;; Automatically pair pairs
(electric-pair-mode 1)

;;; Make C-v and M-v scroll by half a page, maintaining the current screen
;;; position of the cursor.

(defun j/half-window-height ()
  (/ (1- (window-height)) 2))

(defun j/scroll-half-screen-forward ()
  "Scroll half a screen forward, keeping cursor position.
When less than half a screen of lines remains, scroll to the end.

There's a bug here that causes it to sometimes scroll 1 line
short of half a page, which seems to have something to do with
nearing the end of the buffer."
  (interactive)
  (let ((scroll-preserve-screen-position 'always)
        (scroll-error-top-bottom t))
    (scroll-up-command (j/half-window-height))))

(defun j/scroll-half-screen-backward ()
  "Scroll half a screen forward, keeping cursor position.
When less than half a screen of lines remains, scroll to the start."
  (interactive)
  (let ((scroll-preserve-screen-position 'always)
        (scroll-error-top-bottom t))
    (scroll-down-command (j/half-window-height))))

(bind-key "C-v" 'j/scroll-half-screen-forward)
(bind-key "M-v" 'j/scroll-half-screen-backward)

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

;;; Display human-readable file sizes in dired

(setq dired-listing-switches "-alh")

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

;;; Speed up files with very long lines
(setq-default bidi-display-reordering nil)
(setq so-long-threshold 300)
(global-so-long-mode 1)

;;; Set an explicit custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

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
  (let* ((filename (format-time-string "%Y-%m-%d.day")))
    (find-file (expand-file-name (concat "~/life/day/" filename)))))

(defun j/yesterday ()
  (interactive)
  (let* ((day (- (* 60 60 24)))
         (yesterday (time-add (current-time) day))
         (filename (format-time-string "%Y-%m-%d.day" yesterday)))
    (find-file (expand-file-name (concat "~/life/day/" filename)))))

(define-derived-mode j/day-mode text-mode "Day"
  "Major mode for editing dayfiles."
  (auto-fill-mode 1))

(add-to-list 'auto-mode-alist '("\\.day\\'" . j/day-mode))

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

(defvar-local j/align-default-separator ",")

(defun j/align (separator)
  (interactive (list (read-string
                      (format "Separator [%s]: " j/align-default-separator)
                      nil
                      nil
                      j/align-default-separator)))
  (setq-local j/align-default-separator separator)
  (align-regexp (region-beginning)
                (region-end)
                (concat "\\(\\s-*\\) " (regexp-quote separator))
                nil
                0
                t))

;;; Theme ---------------------------------------------------------------------

(defun j/load-theme ()
  (set-frame-parameter nil 'background-mode 'dark)
  (setq doom-spacegrey-brighter-comments t)
  (setq doom-spacegrey-comment-bg nil)
  (setq doom-spacegrey-padded-modeline nil)
  (load-theme 'doom-spacegrey)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (set-frame-font (font-spec :family "Input"
                             :size (if (getenv "IS4K") 22 11))
                  nil
                  t))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (j/load-theme))))

(j/load-theme)

;;; doom-modeline -------------------------------------------------------------

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
  :bind (("C-c C-r" . ivy-resume))
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
     ("d" j/delete-file-action "delete")))
  (ivy-add-actions
   'ivy-switch-buffer
   '(("d" ivy--kill-buffer-action "kill"))))

;;; CTRLF ---------------------------------------------------------------------

(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode 1))

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
         ("C-." . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/mark-previous-symbol-like-this)
         ("C-M-," . mc/unmark-previous-like-this)
         ("C-," . mc/skip-to-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)
         ("C-c C->" . mc/mark-more-like-this-extended)
         ("C-c M-i" . mc/insert-numbers)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (use-package mc-cycle-cursors)
  (use-package mc-hide-unmatched-lines-mode))

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

;;; Better help pages ---------------------------------------------------------

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

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
  (bind-key "M-TAB" 'company-complete-common company-active-map)
  ;; By default, company disables yasnippet bindings while completion is
  ;; active. Since I explicitly don't want TAB to complete, I really just want
  ;; yasnippet to keep working.
  (add-hook 'company-mode-hook
            (lambda ()
              (remove-hook 'yas-keymap-disable-hook 'company--active-p t))))

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

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;; Eldoc ---------------------------------------------------------------------

(use-package eldoc
  :ensure t
  :commands (turn-on-eldoc-mode))

;;; Org -----------------------------------------------------------------------

(defvar j/org-notes-file "~/life/notes.org")
(bind-key "M-C" 'org-capture)

(setq
 org-capture-templates
 `(("b" "Bookmark"
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

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode))

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
  :bind (("C-c s s" . ag)
         ("C-c p s s" . projectile-ag)))

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
  :config (setq wgrep-auto-save-buffer t)
  :bind (("C-c s r" . rg)
         ("C-c p s r" . rg-project)))

;;; Projectile ----------------------------------------------------------------

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode))

(defun j/find-projectile-project (dir)
  (let ((f (locate-dominating-file dir ".projectile")))
    (when f `(projectile . ,f))))

(add-hook 'project-find-functions 'j/find-projectile-project)

(cl-defmethod project-roots ((project (head projectile)))
  (list (cdr project)))

;;; LSP -----------------------------------------------------------------------

(use-package eglot
  :ensure t)

;;; Editorconfig --------------------------------------------------------------

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; dumb-jump -----------------------------------------------------------------

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

;;; Treemacs ------------------------------------------------------------------

(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs treemacs-select-window)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  :bind
  (("M-0"       . treemacs-select-window)
   :map treemacs-mode-map
   ([mouse-1] . treemacs-single-click-expand-action)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;;; Hydra ---------------------------------------------------------------------

(use-package hydra
  :ensure t
  :bind (("C-c r" . hydra-replace/body)
         ("C-c g" . hydra-git/body)
         ("C-c t" . hydra-treemacs/body))
  :config
  (defhydra hydra-replace (:color teal)
    "replace"
    ("s" replace-string "string")
    ("S" query-replace "string (query)")
    ("e" replace-regexp "regexp")
    ("E" query-replace-regexp "regexp (query)"))
  (defhydra hydra-git (:color teal)
    "git"
    ("s" magit-status "status")
    ("b" magit-blame-addition "blame file")
    ("B" magit-blame "blame")
    ("l" magit-log-buffer-file "log file")
    ("L" magit-log "log")
    ("t" git-timemachine "time machine"))
  (defhydra hydra-treemacs (:color teal)
    "treemacs"
    ("t" treemacs "toggle")
    ("w" treemacs-switch-workspace "switch workspace")
    ("a" treemacs-add-project-to-workspace "add project")
    ("s" treemacs-select-window "select")))

;;; Lisp ----------------------------------------------------------------------

(defun j/sly-ivy-completing-read (prompt choices &optional
                                         predicate
                                         require-match
                                         initial-input
                                         hist
                                         def
                                         inherit-input-method)
  "Equivalent to `sly-ido-completing-read', but for ivy. This
  means that if REQUIRE-MATCH is nil, a \"(none)\" option is
  included to return the empty string."
  "Like `ivy-completing-read' but treat REQUIRE-MATCH different.
If REQUIRE-MATCH is nil, offer a \"(none)\" option to return the
empty string."
  (let ((res (ivy-completing-read
              prompt
              (append
               choices
               (unless require-match
                 (list (propertize
                        sly-completing-read-no-match-label
                        'sly--none t))))
              predicate require-match initial-input hist def
              inherit-input-method)))
    (if (get-text-property 0 'sly--none res)
        ""
      res)))

(use-package sly
  :ensure t
  :commands (sly sly-connect)
  :bind ("M-H" . hyperspec-lookup)
  :diminish ")"
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq sly-completing-read-function 'j/sly-ivy-completing-read))

;;; Lua -----------------------------------------------------------------------

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq-default lua-indent-level 2)
  (setq-default lua-indent-nested-block-content-align nil)
  (setq-default lua-indent-close-paren-align nil))

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
         "\\.etlua\\'"
         "\\.ctml\\'"
         "\\.html\\.eex\\'"
         "\\.blade.php\\'")
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
  :mode "\\.php\\'"
  :init
  (add-to-list
   'eglot-server-programs
   `(php-mode . ("php" "vendor/bin/psalm-language-server"))))

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

;;; Vue -----------------------------------------------------------------------

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :init
  (add-to-list
   'eglot-server-programs
   `(vue-mode ,(expand-file-name "vue-language-server/node_modules/vue-language-server/bin/vls"
                                 user-emacs-directory))))

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

;;; Rust ----------------------------------------------------------------------

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package flycheck-rust
  :ensure t
  :after (flycheck rust-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; C -------------------------------------------------------------------------

(setq-default c-default-style "linux"
              c-basic-offset 4)

;;; Elixir --------------------------------------------------------------------

(use-package elixir-mode
  :ensure t
  :mode "\\.ex\\'"
  :init
  (add-to-list
   'eglot-server-programs
   `(elixir-mode "~/bin/blob/elixir-ls/language_server.sh")))
