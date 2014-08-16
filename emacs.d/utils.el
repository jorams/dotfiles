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

(provide 'utils)
