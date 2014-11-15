(require 'company)

(define-key company-active-map (kbd "RET") ())
(define-key company-active-map [return] ())
(define-key company-active-map (kbd "M-RET") 'company-complete-selection)

(provide 'setup-company)
