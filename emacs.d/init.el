;; -*- lexical-binding: t; -*-
;;; Speed up init -------------------------------------------------------------

(defvar j//file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)
            (setq file-name-handler-alist j//file-name-handler-alist)))

;;; Tweak some settings for performance
(setq read-process-output-max (* 1024 1024)
      idle-update-delay 1.0
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;;; Me ------------------------------------------------------------------------

(setq user-full-name "Joram Schrijver"
      user-mail-address "i@joram.io")

(load-file "~/.private.el")

;;; Package installation ------------------------------------------------------

;;; Initialize package.el
(package-initialize nil)

;;; Load packages from MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;;; Backups -------------------------------------------------------------------

(let ((backup-directory (expand-file-name
                         (concat user-emacs-directory "backups/"))))
  (setq backup-directory-alist
        `(("." . ,backup-directory)))
  (setq auto-save-list-file-prefix
        (concat backup-directory "list-"))
  (setq lock-file-name-transforms
        ;; Modified from default auto-save-file-name-transforms
        `(("\\`/\\([^/]*/\\)*\\([^/]*\\)\\'"
           ,(concat temporary-file-directory "\\2") t)))
  (setq tramp-allow-unsafe-temporary-files t))

;;; Visual tweaks -------------------------------------------------------------

;;; Disable the blinking cursor
(blink-cursor-mode -1)

;;; Highlight matching parentheses
(show-paren-mode 1)

;;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;;; Fill column indicator
(global-display-fill-column-indicator-mode 1)

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

;;; Simplify frame title
(setq frame-title-format
      '(multiple-frames
        ("" invocation-name " %b")
        ("" invocation-name)))

;;; Maximize frames by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

(setq isearch-lazy-count t)

;;; Custom startup screen -----------------------------------------------------

(defun j/update-startup-screen (&optional window)
  "Update startup screen content and configure its behavior."
  (with-current-buffer (get-buffer-create "*Startup*")
    (read-only-mode -1)
    (let* ((image (create-image (fancy-splash-image-file)))
           (size (image-size image)))
      (erase-buffer)
      ;; I looked into using an overlay, but this seems easier.
      (insert (make-string (floor (- (window-height) (cdr size)) 2) ?\n)
              (make-string (floor (- (window-width) (car size)) 2) ?\s))
      (insert-image image))
    (setq mode-line-format nil
          cursor-type nil
          truncate-lines t)
    (read-only-mode 1)
    (local-set-key (kbd "q") 'kill-this-buffer)))

(defun j/startup-screen ()
  "Show a startup screen showing only the Emacs splash screen image."
  (j/update-startup-screen (current-buffer))
  (with-current-buffer (get-buffer-create "*Startup*")
    (switch-to-buffer (current-buffer))
    (add-hook 'window-size-change-functions 'j/update-startup-screen 90 t)))

;;; Disable default startup screen
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook #'j/startup-screen)

;;; Basic behaviour -----------------------------------------------------------

;;; Sync selection and X clipboard
(setq x-select-enable-clipboard t)

;;; Save X clipboard before overwriting
(setq save-interprogram-paste-before-kill 512000)

;;; Allow editing of compressed files
(auto-compression-mode t)

;;; Line length 79
(setq-default fill-column 79)

;;; Handle CamelCase nicely
(global-subword-mode 1)

;;; Delete selection before typing with region active
(delete-selection-mode)

;;; Sentences don't end with two spaces
(set-default 'sentence-end-double-space nil)

;;; Disable tab characters
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq tramp-use-ssh-controlmaster-options nil)

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
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;;; Allow multiple recursive minibuffers
(setq-default enable-recursive-minibuffers t)

;;; Enable some commands
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Simplify case conversion in regions
(bind-key "M-u" 'upcase-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-c" 'capitalize-dwim)

;;; Always follow symlinks under version control
(setq vc-follow-symlinks t)

;;; Display human-readable file sizes in dired

(setq dired-listing-switches "-alh")

;;; Speed up files with very long lines
(setq-default bidi-display-reordering nil)

;;; Set an explicit custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;; Disable saving prompts on recompile
(setq compilation-ask-about-save nil
      compilation-save-buffers-predicate 'ignore)

;;; Use hippie-expand instead of dabbrev
(bind-key [remap dabbrev-expand] 'hippie-expand)

;;; Use completing-read to select xref results
(setq xref-show-definitions-function 'xref-show-definitions-completing-read)

(bind-key "C-c d" 'duplicate-dwim)

;;; Enable the context menu
(when (display-graphic-p)
  (context-menu-mode))

(setq split-height-threshold 120)

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
  (auto-fill-mode 1)
  (abbrev-mode 1))

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
                         (concat "linkbox -t " type))
    (kill-new (buffer-substring-no-properties (point-min)
                                              (point-max)))
    (message "%s" (buffer-substring-no-properties (point-min)
                                                  (point-max)))))

(defun j/kdeconnect-get-devices ()
  (with-temp-buffer
    (call-process "kdeconnect-cli"
                  nil
                  t
                  nil
                  "--list-available"
                  "--name-only")
    (split-string (buffer-substring-no-properties (point-min) (point-max)))))

(defun j/send-using-kdeconnect ()
  (interactive)
  "Send selection or buffer to another device using KDE Connect."
  (let* ((start (or (use-region-beginning) (point-min)))
         (end (or (use-region-end) (point-max)))
         (text (buffer-substring-no-properties start end))
         (devices (j/kdeconnect-get-devices))
         (device (completing-read "Device: " devices nil t)))
    (call-process "kdeconnect-cli"
                  nil
                  nil
                  nil
                  "--name"
                  device
                  "--share-text"
                  text)))

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

(defun j/split-list-into-lines ()
  "Split a parameter list across lines.

Assumes point is in the parameter list, not in a nested one expression.

Designed to work in many cases."
  (interactive)
  (backward-up-list)
  (forward-char)
  (unless (member (char-after) '(?\) ?\] ?\}))
    (newline-and-indent)
    (cl-loop
     (cl-case (char-after)
       ((?, ?\;)
        (forward-char)
        (newline-and-indent))
       ((?\) ?\] ?\})
        (newline-and-indent)
        (cl-return)))
     (if (member (char-after) '(?\) ?\] ?\}))
         (cl-return)
       (forward-sexp)))))

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
  (let ((start (if (use-region-p) (region-beginning) 1))
        (end (if (use-region-p) (region-end) (point-max))))
    (align-regexp start
                  end
                  (concat "\\(\\s-*\\) " (regexp-quote separator))
                  nil
                  0
                  t)))

(defun j/json-pretty-print ()
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) 1))
        (end (if (use-region-p) (region-end) (point-max))))
    (json-pretty-print start end)))

(defun j/xml-pretty-print ()
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) 1))
        (end (if (use-region-p) (region-end) (point-max))))
    (shell-command-on-region start end "xmllint --format -"
                             (current-buffer) t)))

(defun j/new-temporary-buffer (arg)
  "Switch to a new temporary buffer. With prefix ARG, switch in other window."
  (interactive "p")
  (if arg
      (switch-to-buffer-other-window (generate-new-buffer "j/temp"))
    (switch-to-buffer (generate-new-buffer "j/temp"))))

(bind-key "C-c n" 'j/new-temporary-buffer)

(defun j/change-string-quotes (pos char)
  "Change the quotes of the string at POS to CHAR."
  (unless (eq 'string (syntax-ppss-context (syntax-ppss pos)))
    (error "Can only switch quotes in a string"))
  (save-excursion
    (goto-char pos)
    (while (eq 'string (syntax-ppss-context (syntax-ppss (point))))
      (backward-char 1))
    (let ((end-of-string (save-excursion (forward-sexp) (point))))
      (delete-char 1)
      (insert char)
      (goto-char end-of-string)
      (delete-char -1)
      (insert char))))

(defun j/change-to-single-quote (pos)
  "Change the string at POS to single quotes."
  (interactive "d")
  (j/change-string-quotes pos ?\'))

(bind-key "C-c '" 'j/change-to-single-quote)

(defun j/change-to-double-quote (pos)
  "Change the string at POS to double quotes."
  (interactive "d")
  (j/change-string-quotes pos ?\"))

(bind-key "C-c \"" 'j/change-to-double-quote)

(defun j/change-to-backtick (pos)
  "Change the string at POS to backticks."
  (interactive "d")
  (j/change-string-quotes pos ?\`))

(bind-key "C-c `" 'j/change-to-backtick)

(defun j/urldecode ()
  "URL encode (unhex) either the region or the entire buffer."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) 1))
         (end (if (use-region-p) (region-end) (point-max)))
         (text (delete-and-extract-region start end)))
    (insert (url-unhex-string text))))

(defun j/urlencode ()
  "URL encode (hexify) either the region or the entire buffer."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) 1))
         (end (if (use-region-p) (region-end) (point-max)))
         (text (delete-and-extract-region start end)))
    (insert (url-hexify-string text))))

;;; Project finding functions

(use-package project
  :config
  (defun j/find-projectile-project (dir)
    "Find project based .projectile file in parent directories of DIR."
    (let ((f (locate-dominating-file dir ".projectile")))
      (when f
        ;; If this is inside a VC repository, we want to detect that while
        ;; staying inside the directory marked with .projectile.
        (if-let ((vc-project (project-try-vc dir)))
            (list 'vc
                  (nth 1 vc-project)    ; backend
                  f)
          (cons 'projectile f)))))

  (add-hook 'project-find-functions 'j/find-projectile-project)

  (cl-defmethod project-root ((project (head projectile)))
    (cdr project))

  (define-key project-prefix-map "m" 'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(defun j/project-terminal ()
  "Open a terminal in the current project root."
  (interactive)
  (start-process "kitty" nil
                 "kitty"
                 "--single-instance"
                 "--detach"
                 "--directory" (expand-file-name (project-root (project-current t)))
                 "--title" "kitty"
                 "--override" "shell=/usr/bin/tmux"))

(with-eval-after-load 'project
  (define-key project-prefix-map "u" #'j/project-terminal)
  (add-to-list 'project-switch-commands '(j/project-terminal "terminal") t))

;;; Theme ---------------------------------------------------------------------

(defun j/set-font ()
  (set-frame-font (font-spec :family "Iosevka Output"
                             :weight 'medium
                             :width 'expanded
                             :size (cond ((getenv "IS4K") 20)
                                         ((getenv "DOTFILE_FONT_RESOLUTION") 16)
                                         (t 12)))
                  nil
                  t))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (j/set-font))))

(use-package auto-dark
  :ensure t
  :init (auto-dark-mode)
  :custom (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi))))

;;; mood-line -----------------------------------------------------------------

(use-package mood-line
  :ensure t
  :hook (after-init . mood-line-mode)
  :config (setq mood-line-format mood-line-format-default-extended))

;;; Undo Tree -----------------------------------------------------------------

(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo)
         ("C-/" . undo-only)
         ("C-?" . undo-redo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t
        vundo-window-max-height 8)
  ;; Increase undo limits to avoid truncation.
  (setq undo-limit 67108864             ; 64 MiB
        undo-strong-limit 100663296     ; 96 MiB
        undo-outer-limit 1006632960))   ; 960 MiB

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;;; Vertico -------------------------------------------------------------------

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (:map vertico-map
              ("C-v" . vertico-scroll-up)
              ("M-v" . vertico-scroll-down)))

(defun j/vertico-kill-line-or-path-component ()
  "Kill the input to the end of the line, or remove the last path component."
  (interactive)
  (if (and (eq 'file (vertico--metadata-get 'category))
           (null (char-after)))
      (let* ((input (minibuffer-contents))
             (slash-pos (cl-search "/" input
                                   :from-end t
                                   :end2 (if (string-suffix-p "/" input)
                                             (1- (length input))
                                           nil))))
        (when slash-pos
          (beginning-of-line)
          (forward-char (1+ slash-pos))
          (delete-region (point) (line-end-position))))
    (delete-region (point) (line-end-position))))

(defun j/vertico-insert-tilde-or-home-directory ()
  "Insert a tilde character, or replace the input with ~/."
  (interactive)
  (if (and (eq 'file (vertico--metadata-get 'category))
           (null (char-after (point))))
      (progn
        (delete-minibuffer-contents)
        (insert "~/"))
    (insert "~")))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("C-j" . vertico-directory-enter)
              ("C-k" . j/vertico-kill-line-or-path-component)
              ("~" . j/vertico-insert-tilde-or-home-directory)))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(defun j/run-terminal-action (dir)
  "Open a terminal in the specified directory."
  (start-process "kitty" nil
                 "kitty"
                 "--single-instance"
                 "--detach"
                 "--directory" (file-name-directory (expand-file-name dir))
                 "--title" "kitty"
                 "--override" "shell=/usr/bin/tmux"))

(defun j/magit-status-action (dir)
  "Run magit-status in the specified directory."
  (require 'magit)
  (let ((magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
    (magit-status (file-name-directory (expand-file-name dir)))))

(defun j/copy-org-heading-property-as-password ()
  "Select document outline heading, then properties to copy the value of.

The value is not entered into the kill ring, but copied using
`interprogram-cut-function'."
  (interactive)
  (consult-outline)
  (unwind-protect
      (cl-loop (let* ((props (delete `("CATEGORY" . ,(org-get-category))
                                     (org-entry-properties nil 'standard)))
                      (prop (completing-read "Property: " props nil t)))
                 (when prop
                   (funcall interprogram-cut-function
                            (cdr (assoc prop props)))
                   nil)))
    (funcall interprogram-cut-function " ")))

(use-package embark
  :ensure t
  :bind (("M-o" . embark-act))
  :config
  (bind-key "u" 'j/run-terminal-action embark-file-map)
  (bind-key "M" 'chmod embark-file-map)
  (bind-key "m" 'j/magit-status-action embark-file-map))

(use-package consult
  :ensure t
  :bind (("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
         ("C-x M-b" . consult-buffer-other-window)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g e" . consult-flymake)
         ("C-c p b" . consult-project-buffer)
         ("M-g M-w" . j/copy-org-heading-property-as-password))
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        orderless-matching-styles '(orderless-literal)))

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
  :init
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  :config
  (setq paredit-space-for-delimiter-predicates
        '((lambda (endp delimiter) nil))))

;;; Colors in compilation mode ------------------------------------------------

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

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
  :bind (("C->" . mc/mark-more-like-this-extended)
         ("C-M->" . j/mc/mark-next-symbol-like-this-extended)
         ("C-<" . j/mc/mark-previous-like-this-extended)
         ("C-M-<" . j/mc/mark-previous-symbol-like-this-extended)
         ("C-c C-<" . mc/mark-all-like-this-dwim)
         ("C-c M-i" . mc/insert-numbers)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C-," . mc/mark-pop)
         :map mc/mark-more-like-this-extended-keymap
         ("C->" . mc/mmlte--down)
         ("C-M->" . j/mc/mmlte--down-symbol)
         ("C-," . mc/mmlte--left)
         ("C-." . mc/mmlte--right)
         ("C-<" . mc/mmlte--up)
         ("C-M-<" . j/mc/mmlte--up-symbol))
  :init
  (use-package mc-cycle-cursors)
  (use-package mc-hide-unmatched-lines-mode)
  :config

  ;; I want to use mark-more-like-this-extended, but still select symbols.
  (defun j/mc/mmlte--up-symbol ()
    (interactive)
    (let ((mc/enclose-search-term 'symbols))
      (mc/mmlte--up)))
  (defun j/mc/mmlte--down-symbol ()
    (interactive)
    (let ((mc/enclose-search-term 'symbols))
      (mc/mmlte--down)))

  ;; I want to use mark-more-like-this-extended but be able to start by moving
  ;; backwards, and/or by selecting symbols.
  (defun j/mc/mark-previous-like-this-extended ()
    "Like mc/mark-more-like-this-extended, but starts moving up instead of down."
    (interactive)
    (mc/mmlte--up)
    (set-transient-map mc/mark-more-like-this-extended-keymap t))
  (defun j/mc/mark-previous-symbol-like-this-extended ()
    "Like mc/mark-more-like-this-extended, but starts moving up instead of down."
    (interactive)
    (j/mc/mmlte--up-symbol)
    (set-transient-map mc/mark-more-like-this-extended-keymap t))
  (defun j/mc/mark-next-symbol-like-this-extended ()
    "Like mc/mark-more-like-this-extended, but starts moving up instead of down."
    (interactive)
    (j/mc/mmlte--down-symbol)
    (set-transient-map mc/mark-more-like-this-extended-keymap t)))

;;; Shift-number --------------------------------------------------------------

(use-package shift-number
  :ensure t
  :bind (("C-M-+" . shift-number-up)
         ("C-M-_" . shift-number-down)))

;;; Aggressive indentation ----------------------------------------------------

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'c++-mode-hook #'aggressive-indent-mode)
  (add-hook 'lua-mode-hook #'aggressive-indent-mode))

;;; Slime-style elisp navigation ----------------------------------------------

(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

;;; Better help pages ---------------------------------------------------------

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;;; Keybind hints -------------------------------------------------------------

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Corfu ---------------------------------------------------------------------

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  ;; By default corfu binds some common keys, so that you need to dismiss
  ;; completions in order to invoke the normal functionality. I want completion
  ;; to help and not get in the way, so I bind these functions to M-<key>
  ;; instead.
  :bind (:map corfu-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("M-TAB" . corfu-complete)
              ("RET" . nil)
              ("M-RET" . corfu-insert)
              ("<remap> <beginning-of-buffer>" . nil)
              ("<remap> <end-of-buffer>" . nil)
              ("<remap> <move-beginning-of-line>" . nil)
              ("C-a" . nil)
              ("<remap> <move-end-of-line>" . nil)
              ("<remap> <next-line>" . nil)
              ("<remap> <previous-line>" . nil))
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.02
        corfu-on-exact-match 'quit))

(use-package corfu-popupinfo
  :init
  (corfu-popupinfo-mode 1)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.5)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;;; Flycheck ------------------------------------------------------------------

(use-package flycheck
  :ensure t
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

(use-package eldoc-box
  :ensure t
  :after eldoc
  :bind ("C-h ." . eldoc-box-help-at-point))

;;; Org -----------------------------------------------------------------------

(defvar j/org-notes-file "~/life/notes.org")

(use-package org
  :ensure nil
  :bind (("M-C" . org-capture))
  :config
  (setq
   org-capture-templates
   `(("b" "Bookmark"
      entry (file+headline ,j/org-notes-file "Bookmarks")
      "* [[%^{URL}][%^{Title}]] %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?")
     ("n" "Note"
      entry (file+headline ,j/org-notes-file "Notes")
      "* %^{Note} %^g\n%T\n\n%?")
     ))
  ;; Increase some text sizes for clarity
  (set-face-attribute 'org-document-title nil :height 1.4)
  (set-face-attribute 'org-level-1 nil :height 1.3)
  (set-face-attribute 'org-level-2 nil :height 1.2)
  (set-face-attribute 'org-level-3 nil :height 1.1))

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
        smtpmail-stream-type 'ssl
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465))

;;; Yasnippet -----------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :config (yas-global-mode)
  (defun j/expand-ssh-host (command)
    "Transform an ssh command line into a snippet for the SSH config."
    (interactive "MCommand: ")
    (if (string-match "ssh \\([a-zA-Z0-9]+\\)@\\(\\S\\+\\)? -p \\([0-9]+\\)"
                      command)
        (let ((snippet (concat "Host $1\n"
                               "    Hostname " (match-string 2 command) "\n"
                               "    Port " (match-string 3 command) "\n"
                               "    User " (match-string 1 command) "\n")))
          (yas-expand-snippet snippet nil nil '((yas-indent-line nil))))
      (error "Command does not match expected pattern."))))

;;; Magit ---------------------------------------------------------------------

(defun j/display-buffer-next-window (buffer alist)
  "Display BUFFER in `next-window', much like `display-buffer-same-window'. ALIST is forwarded."
  (unless (or (window-minibuffer-p (next-window))
              (window-dedicated-p (next-window)))
    (window--display-buffer buffer (next-window) 'reuse alist)))

(defun j/consult-magit-buffer ()
  (interactive)
  (consult-buffer
   `((
      :name "Magit Buffer"
      :narrow ?b
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :default t
      :items ,(lambda () (consult--buffer-query :mode 'magit-status-mode
                                                :sort 'visibility
                                                :as #'buffer-name))))))

(use-package magit
  :ensure t
  :commands (magit-status)
  :bind (("M-M" . magit-status)
         ("C-x M-M" . j/consult-magit-buffer))
  :config
  (setq git-commit-summary-max-length 50)
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (setq fill-column 72))))

(use-package magit-todos
  :ensure t
  :after magit
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

;;; LSP -----------------------------------------------------------------------

(use-package eglot
  :config
  (bind-key "M-p" 'eglot-code-actions eglot-mode-map)
  (bind-key "C-." 'eglot-find-implementation eglot-mode-map)
  (setq eglot-confirm-server-initiated-edits nil)
  (defun eglot--snippet-expansion-fn ()
    (lambda (snippet)
      (yas-expand-snippet
       (s-replace-regexp "${\\([0-9]+\\)}"
                         "$\\1"
                         snippet)))))

;;; Editorconfig --------------------------------------------------------------

(use-package editorconfig
  :ensure t
  :config
  (setq editorconfig-exclude-regexps
        '("COMMIT_EDITMSG"))
  (editorconfig-mode 1))

;;; dumb-jump -----------------------------------------------------------------

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

;;; Transient -----------------------------------------------------------------

(use-package transient
  :ensure t
  :bind (("C-c r" . j/transient-replace)
         ("C-c g" . j/transient-git)
         ("C-c f" . j/transient-format))
  :config
  (transient-define-prefix j/transient-format ()
    ["Format"
     ("j" "json" j/json-pretty-print)
     ("x" "xml" j/xml-pretty-print)
     ("a" "align" j/align)
     ("l" "eglot" eglot-format)
     ("s" "split lines" j/split-list-into-lines)])
  (transient-define-prefix j/transient-git ()
    ["Git"
     ("s" "status" magit-status)
     ("b" "blame file" magit-blame-addition)
     ("B" "blame" magit-blame)
     ("l" "log file" magit-log-buffer-file)
     ("L" "log" magit-log)
     ("t" "time machine" git-timemachine)])
  (transient-define-prefix j/transient-replace ()
    ["Replace"
     ("s" "string" replace-string)
     ("S" "string (query)" query-replace)
     ("e" "regexp" replace-regexp)
     ("E" "regexp (query)" query-replace-regexp)
     ("l" "eglot rename" eglot-rename)
     ("F" "current file" rename-visited-file)]))

;;; Prettier ------------------------------------------------------------------

(use-package prettier
  :ensure t
  :commands (prettier-mode global-prettier-mode))

;;; Lisp ----------------------------------------------------------------------

(use-package sly
  :ensure t
  :commands (sly sly-connect)
  :bind ("M-H" . hyperspec-lookup)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;;; Lua -----------------------------------------------------------------------

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq-default lua-indent-level 2)
  (setq-default lua-indent-nested-block-content-align nil)
  (setq-default lua-indent-close-paren-align nil))

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
         "\\.html\\.leex\\'"
         "\\.blade.php\\'"
         "\\.gohtml\\'")
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook
            #'(lambda ()
                (setq web-mode-enable-auto-pairing nil)
                (setq-local
                 electric-pair-pairs
                 (append electric-pair-pairs '((?% . ?%))))))
  ;; Elixir inside web-mode inside Elixir should be properly highlighted.
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.ex\\'")))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode t))

;;; PHP -----------------------------------------------------------------------

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :init
  (add-to-list
   'eglot-server-programs
   `(php-mode . ("phpactor" "language-server"))))

;;; phpunit -------------------------------------------------------------------

(use-package phpunit
  :ensure t
  :bind (:map php-mode-map
              :package php-mode
              ("C-c ," . j/transient-phpunit))
  :config
  (transient-define-prefix j/transient-phpunit ()
    ["PHPUnit"
     [("a" "all" phpunit-current-project)
      ("g" "group" phpunit-group)]
     [("v" "class" phpunit-current-class)
      ("s" "single" phpunit-current-test)]]))

;;; Javascript ----------------------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode "\\.m?js\\'"
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

;;; Typescript ----------------------------------------------------------------

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (add-to-list 'treesit-language-source-alist
                 '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))

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

(use-package exunit
  :ensure t
  :commands (exunit-mode))

(use-package heex-ts-mode
  :ensure t
  :mode "\\.heex\\'")

(use-package elixir-ts-mode
  :ensure t
  :mode "\\.ex\\'"
  :init
  (add-to-list 'eglot-server-programs `(elixir-ts-mode "elixir-ls"))
  :config
  (defun j/elixir-ts--fill-paragraph (&optional justify)
    "Fill and possibly JUSTIFY paragraph, making sure to stay inside strings."
    (or
     ;; Standard comment handling works fine
     (fill-comment-paragraph justify)
     ;; Inside a string we want to make sure we stay inside the string
     (when-let* ((node (treesit-thing-at-point "string" nil))
                 (start (treesit-node-start node))
                 (end (treesit-node-end node)))
       (save-excursion
         (move-to-left-margin)
         (let ((end (progn (forward-paragraph 1) (min (point) end)))
               (beg (progn (forward-paragraph -1) (max (point) start))))
           (fill-region beg end justify)))
       t)
     ;; We're not in a comment and not in a string, so nothing should happen.
     t))
  (add-hook 'elixir-ts-mode-hook
            (lambda ()
              (setq-local fill-paragraph-function #'j/elixir-ts--fill-paragraph)
              ;; paragraph-{start,separate} are set so that multi-line strings
              ;; keep the quotes on separate lines.
              (setq-local paragraph-start "\f\\|[ \t]*$\\|.*\"\"\"[ \t]*$")
              (setq-local paragraph-separate "[ \t\f]*$\\|.*\"\"\"[ \t]*$")))
  (add-hook 'elixir-ts-mode-hook 'exunit-mode))

;;; Apache --------------------------------------------------------------------

(use-package apache-mode
  :ensure t
  :mode "\\.htaccess\\'")

;;; YAML ----------------------------------------------------------------------

(use-package yaml-ts-mode
  :ensure nil
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;;; Python --------------------------------------------------------------------

(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py[iw]?\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(python "https://github.com/tree-sitter/tree-sitter-python"))))

;;; Caddyfile -----------------------------------------------------------------

(use-package caddyfile-mode
  :ensure t
  :mode ("Caddyfile\\'"))

;;; CSV -----------------------------------------------------------------------

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'"))

;;; Verb HTTP client ----------------------------------------------------------

(use-package verb
  :ensure t
  :after (org)
  :config
  (bind-key "C-c C-r" verb-command-map org-mode-map))

;;; Nginx ---------------------------------------------------------------------

(use-package nginx-mode
  :ensure t
  :commands (nginx-mode))

;;; scfg ----------------------------------------------------------------------

(use-package scfg-mode
  :load-path "lisp/scfg-mode/"
  :mode ("\\.scfg\\'"))

;;; jq ------------------------------------------------------------------------

(use-package jq-mode
  :ensure t
  :mode ("\\.jq\\'"))

;;; vterm ---------------------------------------------------------------------

(use-package vterm
  :ensure t)

;;; GraphQL -------------------------------------------------------------------

(use-package graphql-ts-mode
  ;; To use development code:
  ;; :ensure nil
  ;; :load-path "lisp/graphql-ts-mode/"
  :ensure t
  :mode ("\\.graphql\\'" "\\.gql\\'" "\\.graphqls\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))

;;; Dockerfile ----------------------------------------------------------------

(use-package dockerfile-mode
  :ensure t
  :mode ("/\\(?:Dockerfile\\|Containerfile\\)\\'"))

;;; Go ------------------------------------------------------------------------

(use-package go-ts-mode
  :ensure nil
  :mode ("\\.go\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(go "https://github.com/tree-sitter/tree-sitter-go"))))

(use-package go-mod-ts-mode
  :ensure nil
  :mode ("/go\\.mod\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))))

;;; systemd -------------------------------------------------------------------

(use-package systemd
  :ensure t
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.socket\\'" . systemd-mode)
         ("\\.slice\\'" . systemd-mode)))

;;; HCL -----------------------------------------------------------------------

(use-package hcl-mode
  :ensure t
  :mode (("\\.hcl\\'" . hcl-mode)
         ("\\.nomad\\'" . hcl-mode)
         ("\\.tf\\'" . hcl-mode)
         ("\\.tfvars\\'" . hcl-mode))
  :custom
  (hcl-indent-level 4))

;;; Breadcrumb ----------------------------------------------------------------

(use-package breadcrumb
  :ensure t
  :config (breadcrumb-mode))

;;; Mise ----------------------------------------------------------------------

(use-package mise
  :ensure t
  :config (global-mise-mode))
