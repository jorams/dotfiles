(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-undo-tree-mode)

(provide 'setup-non-evil)
;;; setup-non-evil.el ends here
