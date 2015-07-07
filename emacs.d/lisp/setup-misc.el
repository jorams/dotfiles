;;; Setup-misc --- Configure various modes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Less mode

(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;;; Lua mode

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;; Markdown mode

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))

;;; PHP mode

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;; Arduino mode

(autoload 'arduino-mode "arduino-mode"
  "Major mode for editing Arduino files" t)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

;;; Anaconda mode
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'company-mode)
; Note: Company-anaconda set up below

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(js2r-add-keybindings-with-prefix "C-c C-m")
(add-hook 'js2-mode-hook #'js2-refactor-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; highlight-parentheses.el and highlight-symbol.el
(add-hook 'emacs-lisp-mode-hook       #'highlight-parentheses-mode)
(add-hook 'lisp-mode-hook             #'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook       #'highlight-symbol-mode)
(add-hook 'lisp-mode-hook             #'highlight-symbol-mode)

;;; Ace Jump Mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;; Switch-window
(global-set-key (kbd "C-x o") 'switch-window)

;;; Whitespace cleanup
(global-whitespace-cleanup-mode)

;;; Company mode

(require 'company)
(add-to-list 'company-backends 'company-anaconda)
(define-key company-active-map (kbd "RET") ())
(define-key company-active-map [return] ())
(define-key company-active-map (kbd "M-RET") 'company-complete-selection)

;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit)
(global-set-key (kbd "M-M") 'magit-status)

;;; Projectile
(projectile-global-mode)

;;; Sunrise Commander
(require 'sunrise-commander)

;;; Centered Cursor Mode
(setq-default ccm-recenter-at-end-of-file t)

(provide 'setup-misc)
;;; setup-misc.el ends here
