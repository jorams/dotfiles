(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "sbcl")
(setq slime-repl-history-file
      (expand-file-name "backups/slime-history.eld" user-emacs-directory))
(setq common-lisp-hyperspec-root "file:/home/joram/dev/lisp/HyperSpec/")

(slime-setup '(slime-company))
(slime-setup '(slime-indentation))

(provide 'setup-slime)
