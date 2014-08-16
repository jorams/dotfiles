(require 'color)

(add-to-list 'custom-theme-load-path
			 (expand-file-name (concat user-emacs-directory
									   "themes")))

(load-theme 'distinguished t)
;(load-theme 'base16-default t)

;;; Font settings
;(set-default-font "-adobe-Source Code Pro-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
(set-default-font "-misc-tamsyn-*-*-normal-*-14-*-*-*-*-70-iso8859-1")
(set-frame-font "-misc-tamsyn-*-*-normal-*-14-*-*-*-*-70-iso8859-1")
;(set-face-attribute 'default nil :height 130)

;;; Mode line simplification
(set-face-attribute 'mode-line nil :box nil)

;; Better Company-mode styling
(let ((bg (face-attribute 'default :background)))
  (if (not (string= bg "nil"))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 4)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

;;; Highlight matching parens
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


(provide 'appearance)

