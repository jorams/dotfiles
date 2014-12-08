(require 'evil)
(require 'evil-paredit)

(evil-mode 1)

;;; Global bindings

(evil-global-set-key 'normal (kbd "C-j") 'evil-scroll-down)
(evil-global-set-key 'normal (kbd "C-k") 'evil-scroll-up)

;; This is normally C-o, with jump-forward being <tab>
(evil-global-set-key 'normal (kbd "C-<tab>") 'evil-jump-backward)

;; Insert a blank line without entering insert mode
(defun -evil-inb () (interactive) (evil-insert-newline-below))
(defun -evil-ina () (interactive) (evil-insert-newline-above))

(evil-global-set-key 'normal (kbd "C-o") '-evil-inb)
(evil-global-set-key 'normal (kbd "C-S-o") '-evil-ina)

;;; Mode-specific bindings

(evil-define-key 'normal lisp-interaction-mode-map
  (kbd "<SPC>e") 'eval-last-sexp)
(evil-define-key 'normal lisp-interaction-mode-map
  (kbd "<SPC>e") 'eval-defun)

;; bind M-. and M-, in normal mode
(evil-define-key 'normal lisp-mode-map
  (kbd "M-.") 'slime-edit-definition)
(evil-define-key 'normal lisp-mode-map
  (kbd "M-,") 'slime-pop-find-definition-stack)

;; Dired
(defun better-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(evil-define-key 'normal dired-mode-map "h" 'better-dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
(put 'dired-find-alternate-file 'disabled nil)

;; And add a key to look something up in the HyperSpec
(global-set-key (kbd "M-H") 'common-lisp-hyperspec)

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-mode-hook 'evil-paredit-mode)

;;; Multiple cursors

;; bind <SPC>m in visual mode to create multiple cursors
(define-key evil-visual-state-map (kbd "<SPC>M") 'mc/edit-lines)
;; This one doesn't work that nicely
(define-key evil-visual-state-map (kbd "<SPC>m") 'mc/mark-all-like-this)

;; bind <SPC>m [something] to create multiple cursors
;; We first have to get rid of the default <SPC> binding
(define-key evil-normal-state-map (kbd "<SPC>mn") 'mc/mark-next-like-this)
(define-key evil-normal-state-map (kbd "<SPC>mp") 'mc/mark-previous-like-this)

;; Exit insert mode with C-g
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

(defun delete-trailing-whitespace-on-current-line ()
  (delete-trailing-whitespace (line-beginning-position) (line-end-position)))
(add-hook 'evil-insert-state-exit-hook #'delete-trailing-whitespace-on-current-line)

;;; Org-mode
(evil-define-key 'normal org-mode-map
  (kbd "<tab>") 'org-cycle
  (kbd "<SPC>l") 'org-metaright
  (kbd "<SPC>h") 'org-metaleft
  (kbd "<SPC>j") 'org-metadown
  (kbd "<SPC>k") 'org-metaup
  (kbd "C-=") 'org-todo
  (kbd "<SPC>O") 'org-insert-heading
  (kbd "<SPC>o") 'org-insert-heading-after-current
  (kbd "C-j") 'org-forward-same-level
  (kbd "C-k") 'org-backward-same-level)

(evil-define-key 'insert org-mode-map
  (kbd "C-=") 'org-todo
  (kbd "M-l") 'org-metaright
  (kbd "M-h") 'org-metaleft
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M-O") 'org-insert-heading
  (kbd "M-o") 'org-insert-heading-after-current)

(provide 'setup-evil)
;;; setup-evil.el ends here
