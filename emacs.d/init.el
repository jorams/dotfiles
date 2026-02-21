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

;;; Startup template

(add-hook
 'after-init-hook
 (lambda ()
   (let ((file (expand-file-name "startup.el" user-emacs-directory)))
     (if (file-exists-p file)
         (load-file file)
       (add-hook 'emacs-startup-hook #'j/startup-screen)))))

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

(defvar-local j/compilation-file-strip-absolute-regexp nil)

(define-advice compilation-find-file (:around (orig marker filename directory &rest formats) j/strip-absolute-regexp)
  "Allow compilation output filenames to change before opening.

The regexp should be set buffer-locally as
j/compilation-file-strip-absolute-regexp. Any match for this regex will
be removed from the filename.

An example use for this is when the output references file names of the current
directory mounted inside a container. The regexp could be something like ^/app/"
  (if (and (file-name-absolute-p filename) j/compilation-file-strip-absolute-regexp)
      (let ((filename (replace-regexp-in-string j/compilation-file-strip-absolute-regexp "" filename)))
        (apply orig marker filename directory formats))
    (apply orig marker filename directory formats)))

;;; Compilation mode doesn't normally load variables from .dir-locals.el
(add-hook 'compilation-mode-hook 'hack-dir-local-variables-non-file-buffer)

;;; Project finding functions

(defun j/project-terminal ()
  "Open a terminal in the current project root."
  (interactive)
  (start-process "kitty" nil
                 "kitty"
                 "--single-instance"
                 "--detach"
                 "--directory" (expand-file-name (project-root (project-current t)))
                 "--title" (project-name (project-current))
                 "--override" "shell=/usr/bin/tmux"))

(defun j/project-vterm ()
  "Open a vterm in the current project root or switch to an existing vterm."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if vterm-buffer
        (pop-to-buffer vterm-buffer)
      (vterm-other-window vterm-buffer-name))))

(defun j/project-tab-magit-project-status ()
  "Open a new tab with the magit status of the project."
  (interactive)
  (tab-new)
  (tab-rename (project-name (project-current)))
  (magit-project-status)
  (delete-other-windows))

(defun j/magit-project-status-only ()
  "Open a magit status buffer and make it the only window."
  (interactive)
  (magit-project-status)
  (delete-other-windows))

(defun j/project-copy-buffer-file-name ()
  "Copy the project-relative file name of the current buffer."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (project-root (project-root (project-current)))
         (relative-name (file-relative-name file-name project-root)))
    (kill-new relative-name)
    (message relative-name)))

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
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)

  (define-key project-prefix-map "M" 'j/magit-project-status-only)
  (add-to-list 'project-switch-commands '(j/magit-project-status-only "Magit (only)") t)

  (define-key project-prefix-map "u" #'j/project-terminal)
  (add-to-list 'project-switch-commands '(j/project-terminal "terminal") t)

  (define-key project-prefix-map "v" #'j/project-vterm)
  (add-to-list 'project-switch-commands '(j/project-vterm "vterm") t)

  (define-key project-prefix-map "t" #'j/project-tab-magit-project-status)
  (add-to-list 'project-switch-commands '(j/project-tab-magit-project-status "tab-magit") t)

  (define-key project-prefix-map (kbd "M-w") #'j/project-copy-buffer-file-name)

  ;; Project-specific transients
  (define-key project-prefix-map "/" 'j/project-transient)
  (add-to-list 'project-switch-commands '(j/project-transient "Transient") t))

;;; Project-specific transients -----------------------------------------------
;;; Transients offering easy access to common commands you can run in the
;;; context of a project. The actual commands and transients are mostly defined
;;; in project-transients.el.
;;;
;;; project-transients.el should look somewhat like the following. Note that
;;; more specific transients are defined later.
;;
;; ;;; a --- Project transients -*- lexical-binding: t; -*-
;; ;;; Commentary:
;; ;;; Code:
;;
;; (require 'j/project-transients
;;          (expand-file-name "init.el" user-emacs-directory))
;;
;; (j/define-project-vterm j---mix-run "run"
;;   "mix run")
;;
;; (j/define-project-transient-prefix (:indicator-file "mix.lock") ()
;;   ["Elixir"
;;    ("r" "run" j---mix-run)])
;;
;; (j/define-project-transient-prefix (:root "~/dev/some-project/") ()
;;   ["Main"
;;    ("r" "run" j---mix-run)])
;;
;; ;;; project-transients.el ends here

(defvar j/project-transient-prefixes ())

(cl-defmacro j/define-project-transient-prefix ((&key root indicator-file)
                                                &body body)
  "Define project transient for ROOT or INDICATOR-FILE.
See transient-define-prefix for BODY.

ROOT is a directory in which the transient is active. This does not have
to be the project root itself, it can also be a directory higher up in
the directory tree, which will make it apply to all projects under it
that don't have their own.

INDICATOR-FILE is a file name that, if found in the project root, makes
this transient apply to the project. It can also be a list of file
names. Transients defined with ROOT take precedence over those defined
with INDICATOR-FILE."
  (declare (indent defun))
  (let* ((subname (or root
                      (if (stringp indicator-file)
                          indicator-file
                        (car indicator-file))))
         (transient-name (intern (concat "j/project-transient/" subname))))
    `(progn
       (add-to-list 'j/project-transient-prefixes '((:root ,root :indicator-file ,indicator-file) ,transient-name))
       (transient-define-prefix ,transient-name ,@body))))

(defvar j/project-transient-temp-buffer-name "*j/temporary-project*")

(defun j/project-transient-delete-temporary-buffer ()
  "Delete temporary buffer created for the project transient.
A temporary buffer is created when opening a project transient from
outside the relevant project, to ensure suffix commands in the transient
run inside said project."
  (when (get-buffer j/project-transient-temp-buffer-name)
    (kill-buffer j/project-transient-temp-buffer-name)))

(defun j/project-transient-call-in-project (fun project)
  "Call FUN, a transient, in the context of PROJECT."
  (unless (eq project (project-current nil default-directory))
    ;; Any suffix commands will end up running outside the context of the
    ;; project, so pop to a temporary buffer in the correct project.
    (add-hook 'transient-post-exit-hook #'j/project-transient-delete-temporary-buffer)
    (let ((default-directory (project-root project)))
      (pop-to-buffer-same-window j/project-transient-temp-buffer-name)))
  (funcall-interactively fun))

(defun j/project-transient ()
  "Run a possible transient for the current project."
  (interactive)
  (j/load-project-transients)
  (let* ((current-project (project-current t))
         (root (project-root current-project))
         (root-prefix
          (cl-loop for (transient-indicator transient-name)
                   in j/project-transient-prefixes
                   for transient-root = (cl-getf transient-indicator :root)
                   when (and transient-root
                             (string-prefix-p (expand-file-name root)
                                              (expand-file-name transient-root)))
                   return transient-name)))
    (if root-prefix
        (j/project-transient-call-in-project root-prefix current-project)
      (let ((indicator-file-prefix
             (cl-loop for (transient-indicator transient-name)
                      in j/project-transient-prefixes
                      for transient-indicator-file = (cl-getf transient-indicator :indicator-file)
                      when (or (and transient-indicator-file
                                    (stringp transient-indicator-file)
                                    (file-exists-p (expand-file-name transient-indicator-file root)))
                               (and transient-indicator-file
                                    (listp transient-indicator-file)
                                    (cl-find-if
                                     (lambda (f)
                                       (file-exists-p (expand-file-name f root)))
                                     transient-indicator-file)))
                      return transient-name)))
        (if indicator-file-prefix
            (j/project-transient-call-in-project indicator-file-prefix current-project))))))

(cl-defmacro j/define-project-shell-command (name command)
  "Define NAME to run COMMAND in project."
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     (j/shell-command-in-project ,command)))

(cl-defmacro j/define-project-compile (name buffer-name command)
  "Define NAME to run COMMAND as compilation in project buffer BUFFER-NAME."
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     (j/compile-in-project ,command ,buffer-name)))

(cl-defmacro j/define-project-vterm (name buffer-name command)
  "Define NAME to run COMMAND in vterm in project buffer BUFFER-NAME."
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     (j/run-in-vterm-in-project ,command ,buffer-name)))

(defun j/shell-command-in-project (command)
  "Run COMMAND in the current project root."
  (let ((default-directory (project-root (project-current t))))
    (shell-command command)))

(defun j/compile-in-project (command buffer-name)
  "Run COMMAND in the current project root.
BUFFER-NAME is used to generate a compilation buffer name."
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (lambda (_)
           (concat (project-prefixed-buffer-name buffer-name))))
        (compilation-read-command nil))
    (compile command)))

(defun j/run-in-vterm-in-project (command buffer-name)
  "Run COMMAND in the current project root in a vterm.
BUFFER-NAME is used to generate a buffer name."
  (let* ((default-directory (project-root (project-current t)))
         (buffer-name (project-prefixed-buffer-name buffer-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (pop-to-buffer buffer)
      (with-current-buffer (vterm-other-window buffer-name)
        (vterm-insert command)
        (vterm-send-return)))))

(defun j/load-project-transients ()
  "Load project-specific transients from project-transients.el."
  (unless (featurep 'j/project-transients)
    (provide 'j/project-transients)
    (require 'transient)
    (let ((file (expand-file-name "project-transients.el"
                                  user-emacs-directory)))
      (when (file-exists-p file)
        (load-file file)))))

;;; Window rules --------------------------------------------------------------

(add-to-list 'display-buffer-alist
             '((or (major-mode . vterm-mode)
                   (derived-mode . compilation-mode)
                   "-vterm\\*")
               display-buffer-in-side-window
               (side . right)
               (window-width . 80)))

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

(j/set-font)

(use-package auto-dark
  :ensure t
  :init (auto-dark-mode)
  :custom (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi)))
  :config
  (add-hook 'auto-dark-dark-mode-hook
            (lambda ()
              (set-face-attribute 'mode-line-inactive nil
                                  :box nil
                                  :foreground "#969696"
                                  :background "#1d2235"
                                  :overline "#4a4f69")
              (set-face-attribute 'mode-line-active nil
                                  :box nil
                                  :foreground "#ffffff"
                                  :background "#1d2235"
                                  :overline "#4a4f69")))
  (add-hook 'auto-dark-light-mode-hook
            (lambda ()
              (set-face-attribute 'mode-line-inactive nil
                                  :box nil
                                  :foreground "#656565"
                                  :background "#f2f2f2"
                                  :overline "#a0a0a0")
              (set-face-attribute 'mode-line-active nil
                                  :box nil
                                  :foreground "#000000"
                                  :background "#F2F2F2"
                                  :overline "#a0a0a0"))))

;;; Flymake -------------------------------------------------------------------

(use-package flymake
  :config
  (setq flymake-show-diagnostics-at-end-of-line t))

;;; Custom mode line ----------------------------------------------------------

(setq-default mode-line-format
              '("%e "
                ;; Keyboard macro recording?
                (:eval
                 (when (and defining-kbd-macro
                            (mode-line-window-selected-p))
                   (propertize "rec " 'face 'breakpoint-enabled)))
                ;; Buffer readonly?
                (:eval
                 (when buffer-read-only
                   (propertize "# " 'face 'font-lock-keyword-face)))
                ;; Buffer remote?
                (:eval
                 (when (and default-directory
                            (file-remote-p default-directory))
                   (propertize "@ " 'face 'warning)))
                ;; Buffer narrowed?
                (:eval
                 (when (buffer-narrowed-p)
                   (propertize "N " 'face 'warning)))
                ;; Buffer modified?
                (:eval
                 (when (and (buffer-file-name (buffer-base-buffer))
                            (buffer-modified-p))
                   (propertize "* " 'face 'error)))
                ;; Current project
                (:eval
                 (when-let* ((project (project-current nil))
                             (name (project-name project)))
                   (propertize (format "%s / " name)
                               'face 'ansi-color-faint)))
                ;; Buffer name
                (:eval
                 (propertize "%b" 'face 'bold))
                ;; Current line, current character in line
                (:eval
                 (if (buffer-file-name (buffer-base-buffer))
                     (propertize ":%l:%c " 'face 'ansi-color-faint)
                   " "))
                ;; Region lines and characters
                (:eval
                 (when (use-region-p)
                   (propertize (format "%sL/%sC "
                                       (count-lines (region-beginning)
                                                    (region-end))
                                       (- (region-end) (region-beginning)))
                               'face 'font-lock-variable-use-face)))
                ;; Line endings if not LF
                (:eval
                 (when buffer-file-coding-system
                   (propertize
                    (pcase (coding-system-eol-type buffer-file-coding-system)
                      (0 "")
                      (1 "CRLF ")
                      (2 "CR "))
                    'face 'warning)))
                ;; Encoding if not UTF-8
                (:eval
                 (when buffer-file-coding-system
                   (let ((plist (coding-system-plist
                                 buffer-file-coding-system)))
                     (unless (memq (plist-get plist :category)
                                   '(coding-category-undecided
                                     coding-category-utf-8))
                       (propertize (upcase
                                    (symbol-name (plist-get plist :name)))
                                   'face 'warning)))))
                ;; Multiple cursors
                (:eval
                 (when (bound-and-true-p multiple-cursors-mode)
                   (propertize (format "%s cursors " (mc/num-cursors))
                               'face 'font-lock-comment-face)))
                ;; Right align the rest
                mode-line-format-right-align
                ;; Flymake status
                (:eval
                 (when (bound-and-true-p flymake-mode)
                   flymake-mode-line-counters))
                " "
                ;; Leaving out mode-name because it's usually obvious
                mode-line-process
                mode-line-misc-info
                "  "))

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

;;; Tabs ----------------------------------------------------------------------

(use-package emacs
  :bind (("C-S-n" . tab-next)
         ("C-S-p" . tab-previous)))

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
  (set-face-attribute 'org-level-3 nil :height 1.1)
  (add-to-list 'browse-url-handlers
               '("https://\.*\\.atlassian\\.net/\.*" . browse-url-chromium)))

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
  (setq eglot-confirm-server-initiated-edits nil))

;;; DAP -----------------------------------------------------------------------

(use-package dape
  :ensure t)

(use-package repeat
  :config
  (repeat-mode))

;;; Editorconfig --------------------------------------------------------------

(use-package editorconfig
  :config
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
         ("C-c f" . j/transient-format)
         ("C-c /" . j/project-transient))
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

(defvar j/markdown-live-preview-temporary-file-name nil)

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.md\\'")
  :config
  (define-advice markdown-live-preview-get-filename (:override () j/markdown-live-preview-temporary-file)
    "Make markdown-live-preview-mode use a temporary file for exports."
    (unless j/markdown-live-preview-temporary-file-name
      (setq-local j/markdown-live-preview-temporary-file-name (make-temp-file "markdown-live-" nil ".html")))
    j/markdown-live-preview-temporary-file-name))

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

(defun j/php-manual-lookup (name)
  "Look up NAME on php.net."
  (interactive
   (list (when-let* ((thing (treesit-thing-at-point "" 'nested)))
           (treesit-node-text thing))))
  (when name
    (browse-url (concat "https://www.php.net/" name))))

(use-package php-ts-mode
  :mode "\\.php\\'"
  :bind (:map php-ts-mode-map
              :package php-ts-mode
              ("C-c C-f" . j/php-manual-lookup))
  :config
  (add-hook 'php-ts-mode-hook
            (lambda ()
              (setq imenu-create-index-function #'treesit-simple-imenu)))
  :init
  (add-to-list
   'eglot-server-programs
   `(php-ts-mode . ("phpactor" "language-server"))))

;;; phpunit -------------------------------------------------------------------

(use-package phpunit
  :ensure t
  :bind (:map php-ts-mode-map
              :package php-ts-mode
              ("C-c ," . j/transient-phpunit))
  :config
  (defvar phpunit-last-suite-cache nil)

  (defun phpunit--get-last-suite (path)
    "Get last suite cache by `PATH'."
    (if (null phpunit-last-suite-cache)
        nil
      (gethash path phpunit-last-suite-cache nil)))

  (defun phpunit--put-last-suite (suite path)
    "Put last suite `SUITE' cache by `PATH'."
    (unless phpunit-last-suite-cache
      (setq phpunit-last-suite-cache (make-hash-table :test 'equal)))
    (puthash path suite phpunit-last-suite-cache))

  (defun phpunit--listing-suites ()
    "Return list of test suites."
    (let ((phpunit-output (phpunit--execute "--list-suites")))
      (with-temp-buffer
        (insert phpunit-output)
        (goto-char (point-min))
        (search-forward "Available test suites")
        (move-beginning-of-line 1)
        (forward-line)
        (cl-loop
         for line in (s-split "\n" (buffer-substring-no-properties (point) (point-max)))
         if (string-match " - \\(.+?\\)\\(?: ([0-9]+ tests?)\\)?$" line)
         collect (print (match-string 1 line))))))

  (defun phpunit-suite (use-last-suite &optional suite)
    "Launch PHPUnit for suite."
    (interactive "p")
    (let* ((current-root-directory (phpunit-get-root-directory))
           (last-suite (phpunit--get-last-suite current-root-directory)))
      (when (called-interactively-p 'interactive)
        (setq use-last-suite (eq use-last-suite 1))
        (setq suite (if (and use-last-suite last-suite)
                        last-suite
                      (completing-read-multiple "PHPUnit suite: " (phpunit--listing-suites)))))
      (phpunit-run (format "--testsuite %s" (string-join suite ",")))
      (phpunit--put-last-suite suite current-root-directory)))

  (defun j/phpunit-last-suite ()
    (interactive)
    (funcall-interactively #'phpunit-suite 1))

  (defun j/phpunit-select-suite ()
    (interactive)
    (funcall-interactively #'phpunit-suite nil))

  (transient-define-prefix j/transient-phpunit ()
    ["PHPUnit"
     [("a" "all" phpunit-current-project)
      ("t" "suite" j/phpunit-last-suite)
      ("T" "select suite" j/phpunit-select-suite)]
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
  :mode ("\\.[hl]?eex\\'"))

(defun j/local-hexdocs ()
  "Open local documentation for a selected hex dependency."
  (interactive)
  (let ((mix-dir (locate-dominating-file default-directory "mix.exs")))
    (unless mix-dir
      (error "Can't find a mix.exs file"))
    (let* ((default-directory mix-dir)
           (deps-output
            (with-temp-buffer
              (call-process "mix" nil t nil "deps")
              (buffer-string)))
           (regex
            "\\* .+ (Hex package).+\n  locked at [^ ]+? (\\([^ ]+?\\))")
           (hex-deps (cl-loop
                      for start = 0 then (match-end 0)
                      for matches = (string-match regex deps-output start)
                      do (message "%s" start)
                      while matches
                      collect (match-string 1 deps-output)))
           (deps (completing-read-multiple "Open documentation for: "
                                           (cons "elixir" hex-deps)
                                           nil
                                           t)))
      (dolist (dep deps)
        (start-process (format "mix hex.docs offline %s" dep) nil
                       "mix" "hex.docs" "offline" dep)))))

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock")
  :init
  (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode) "expert" "--stdio"))
  :config
  (add-hook 'elixir-ts-mode-hook
            (lambda ()
              ;; Something more complete than this should be added to
              ;; elixir-ts-mode proper.
              (setq-local treesit-thing-settings
                          `((elixir (text ,(regexp-opt '("comment" "quoted_content"))))))
              ;; paragraph-{start,separate} are set so that multi-line strings
              ;; keep the quotes on separate lines.
              (setq-local paragraph-start "\f\\|[ \t]*$\\|.*\"\"\"[ \t]*$")
              (setq-local paragraph-separate "[ \t\f]*$\\|.*\"\"\"[ \t]*$")))
  (add-hook 'elixir-ts-mode-hook 'exunit-mode)
  (bind-key "C-c h" 'j/local-hexdocs elixir-ts-mode-map))

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
  :ensure t
  :config
  (add-hook 'vterm-mode-hook
            (lambda () (setq-local show-trailing-whitespace nil))))

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
                 '(go "https://github.com/tree-sitter/tree-sitter-go")))
  :config
  (setq go-ts-mode-indent-offset 4))

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
