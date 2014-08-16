(autoload 'arduino-mode "arduino-mode"
  "Major mode for editing Arduino files" t)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

(provide 'setup-arduino)
