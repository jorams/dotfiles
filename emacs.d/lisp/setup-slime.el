(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "sbcl")
(setq slime-repl-history-file
      (expand-file-name "backups/slime-history.eld" user-emacs-directory))
(setq common-lisp-hyperspec-root "file:///home/joram/.dump/HyperSpec/")

(setq slime-company-completion 'fuzzy)

;; company-quickhelp will request a documentation buffer for any candidate, but
;; some candidates are package names. This will cause the debugger to be opened
;; due to the way swank parses the symbol requested for description. This is a
;; crude hack to work around that.
(defadvice slime-company--doc-buffer (around package-guard (candidate) activate)
  (unless (string-match-p ":$" candidate)
    ad-do-it))

(slime-setup '(slime-fancy slime-company slime-indentation slime-asdf))

;; Add a key to look something up in the HyperSpec
(global-set-key (kbd "M-H") 'common-lisp-hyperspec)

(provide 'setup-slime)
