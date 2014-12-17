(require 'powerline)

(defun powerline-my-theme ()
  "Powerline theme, originally based on powerline-center-evil-theme."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
             (lhs (append (list (powerline-raw "%*" nil 'l))
                          ;; Evil
                          (if evil-mode
                              (list (funcall separator-left face1 face2)
                                    (powerline-raw evil-mode-line-tag face2 'l)
                                    (powerline-raw " " face2)
                                    (funcall separator-left face2 face1)))
                          ;; Buffer name
                          (list (powerline-buffer-id nil 'l))
                          ;; Location in file
                          (list
                           (funcall separator-left face1 face2)
                           (powerline-raw "%4l" face2 'r)
                           (powerline-raw ":" face2)
                           (powerline-raw "%3c" face2 'r)
                           (powerline-raw " " face2)
                           (powerline-raw "%6p" face2 'r)
                           (funcall separator-left face2 face1))
                          ;; Version control
                          (list (powerline-narrow face1 'l)
                                (powerline-vc face1)
                                (powerline-raw " " face1))))
             (rhs (append
                   ;; Major mode and process
                   (list (when (boundp 'erc-modified-channels-object)
                           (powerline-raw erc-modified-channels-object face2 'l))
                         (funcall separator-right face1 face2)
                         (powerline-major-mode face2 'l)
                         (powerline-raw " " face2)
                         (funcall separator-right face2 face1)
                         (powerline-process face1)
                         (powerline-raw " " face1))
                   ;; Minor modes
                   (if (split-string (format-mode-line minor-mode-alist))
                       (list (powerline-minor-modes face1 'l)
                             (powerline-raw " " face1))))))
        (concat (powerline-render lhs)
                (powerline-fill face1 (powerline-width rhs))
                (powerline-render rhs)))))))

(powerline-my-theme)

(provide 'setup-powerline)
