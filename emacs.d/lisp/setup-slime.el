(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "sbcl")
(setq slime-repl-history-file
      (expand-file-name "backups/slime-history.eld" user-emacs-directory))
(setq common-lisp-hyperspec-root "file:///home/joram/.dump/HyperSpec/")

(setq slime-company-completion 'fuzzy)
(slime-setup '(slime-fancy slime-company slime-indentation slime-asdf))

;; Add a key to look something up in the HyperSpec
(global-set-key (kbd "M-H") 'common-lisp-hyperspec)

(provide 'setup-slime)
