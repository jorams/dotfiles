(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "sbcl")

(setq common-lisp-hyperspec-root "file:/home/joram/dev/lisp/HyperSpec/")

(slime-setup '(slime-company))

(slime-setup '(slime-indentation))

(define-common-lisp-style "evl"
  "Style used in the evl sources. Based on the SBCL style, with subclause aware
loop indentation and some evl specific constructs"
  (:inherit "sbcl")
  (:variables
   (lisp-loop-indent-subclauses t))
  (:indentation
   (defmethod* (as defmethod))))

(provide 'setup-slime)
