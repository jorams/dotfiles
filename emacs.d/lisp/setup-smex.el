(setq smex-save-file
      (expand-file-name "backups/smex-items" user-emacs-directory))

(require 'smex)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'setup-smex)