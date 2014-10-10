(defun wrap/hard ()
  (interactive)
  (auto-fill-mode 'toggle))

(defun wrap/soft ()
  (interactive)
  (longlines-mode 'toggle))

(defun colorcolumn ()
  (interactive)
  (fci-mode 'toggle))

(defun day ()
  (interactive)
  (find-file (expand-file-name (concat "~/life/day/"
				       (format-time-string "%Y-%m-%d.day"))))
  (auto-fill-mode 1))

;;; Taken from here:
;;; http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defun compile-without-asking ()
  (interactive)
  (save-buffer)
  (compile compile-command))

(global-set-key (kbd "M-N") 'compile-without-asking)

(provide 'utils)
