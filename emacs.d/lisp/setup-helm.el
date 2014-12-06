;;; Setup-Helm --- Set up Helm mode and settings
(require 'helm-config)

(helm-mode 1)

(setq helm-split-window-in-side-p       t
      helm-move-to-line-cycle-in-source t)

;;; Bindings to access Helm

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;; Bindings in Helm buffers

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-<tab>") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-j") 'helm-execute-persistent-action)

(defun helm-magit-status (candidate)
  (interactive)
  (magit-status (file-name-directory candidate)))

(define-key helm-find-files-map (kbd "M-M")
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-quit-and-execute-action 'helm-magit-status))))

(provide 'setup-helm)
;;; setup-helm.el ends here
