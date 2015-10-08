;;; Appearance --- Make Emacs look nicer

(require 'color)

(add-to-list 'custom-theme-load-path
			 (expand-file-name (concat user-emacs-directory
									   "themes")))

(defun j/load-theme ()
  (load-theme 'my-darktooth t)
  (set-frame-font "-Misc-Tamsyn-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1"
                  nil
                  t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (j/load-theme))))
  (j/load-theme))


;; Highlight matching parens
(show-paren-mode 1)

;;; Transparency
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
	   (cadr (frame-parameter nil 'alpha))
	   100)
	  (set-frame-parameter nil 'alpha '(100 100))
	(set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
(set-frame-parameter nil 'alpha '(100 100))

;; Show excess whitespace
(setq-default show-trailing-whitespace t)

;; ...except in the minibuffer
(defun minibuffer-hide-whitespace ()
  (setq show-trailing-whitespace nil))
(add-hook 'minibuffer-inactive-mode-hook #'minibuffer-hide-whitespace)


;; Make it easier to spot excess newslines at the end of a buffer
(setq-default indicate-empty-lines t)

;;; Nicer buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Optional fill column indicator
(require 'fill-column-indicator)

(provide 'appearance)

;;; appearance.el ends here
