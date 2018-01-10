-*- coding: utf-8 -*-

(defun osx-paste ()
  (interactive)
  (insert (shell-command-to-string "pbpaste"))
)
(global-set-key "\C-v" 'osx-paste)
(message "osx-paste defined and assigned to \C-v")
