;;; Setup-org --- Configure org-mode

(require 'org-passwords)

(setq org-default-notes-file "~/life/notes.org")
(global-set-key (kbd "M-C") 'org-capture)

(setq
 org-capture-templates
 `(("p" "password"
    entry (file ,org-passwords-file)
    "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p")
   ("b" "Bookmark"
    entry (file+headline "" "Bookmarks")
    "* [[%^{URL}][%^{Title}]] %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?")
   ))

(provide 'setup-org)
;;; setup-org.el ends here
