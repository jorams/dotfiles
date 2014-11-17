;;; Appearance --- Make Emacs look nicer

(require 'color)

(add-to-list 'custom-theme-load-path
			 (expand-file-name (concat user-emacs-directory
									   "themes")))

(load-theme 'ujelly t)

;; Font settings
(set-frame-font "-misc-tamsyn-*-*-normal-*-14-*-*-*-*-70-iso8859-1" nil t)

;; Better Company-mode styling
(let ((bg (face-attribute 'default :background)))
  (if (not (string= bg "nil"))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 4)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

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

;;; Nicer scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)

;;; Optional fill column indicator
(require 'fill-column-indicator)

(provide 'appearance)

;;; appearance.el ends here
