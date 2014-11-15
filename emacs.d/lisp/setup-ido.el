(require 'ido)

(setq ido-save-directory-list-file
      (expand-file-name "backups/ido.last" user-emacs-directory))

(ido-mode 1)

;;; Everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;;; ido at point
(require 'ido-at-point)
(ido-at-point-mode)

;;; flx fuzzy matching
(require 'flx-ido)
(flx-ido-mode 1)

(eval-after-load 'ido
  (progn (require 'ido-vertical-mode)
         (ido-vertical-mode)))

(provide 'setup-ido)
