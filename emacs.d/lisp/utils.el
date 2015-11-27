(defun wrap/hard ()
  (interactive)
  (auto-fill-mode 'toggle))

(defun wrap/soft ()
  (interactive)
  (visual-fill-column-mode 'toggle)
  (visual-line-mode 'toggle))

(defun colorcolumn ()
  (interactive)
  (fci-mode 'toggle))

(defun day ()
  (interactive)
  (find-file (expand-file-name (concat "~/life/day/"
				       (format-time-string "%Y-%m-%d.day"))))
  (auto-fill-mode 1))

(defun fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) fill-column)
      (insert-char char))))

(defun fill-to-end-- ()
  (interactive)
  (fill-to-end ?\-))

(global-set-key (kbd "M-F") 'fill-to-end--)

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

;;; Smarter C-a. From http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun j/align-on-single-quote ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\) '" nil 0 t))

(defun transmission-add-url (torrent &optional directory)
  "Add a torrent from a URL. This to work around an issue where
Helm's file input will prepend the current directory to the
entered path, which is a URL in this case."
  (interactive (list (read-string "Add torrent URL: ")
                     (if current-prefix-arg
                         (read-directory-name "Target directory: "))))
  (let ((arguments
         (append `(:filename ,torrent)
                 (list :download-dir directory))))
    (let-alist (transmission-request "torrent-add" arguments)
      (pcase .result
        ("success"
         (or (and .arguments.torrent-added.name
                  (message "Added %s" .arguments.torrent-added.name))
             (and .arguments.torrent-duplicate.name
                  (user-error "Already added %s"
                              .arguments.torrent-duplicate.name))))
        (_ (user-error .result))))))

(defun torr ()
  (interactive)
  (transmission)
  (define-key transmission-mode-map (kbd "A") 'transmission-add-url))

(provide 'utils)
