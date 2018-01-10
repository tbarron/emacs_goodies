;;;; ===============================================================
;;;; Key bindings
;;;; ===============================================================
(global-set-key "\M-b" 'eval-buffer)
(global-set-key "\M-\\" 'indent-region)
(global-set-key "\C-y" 'clipboard-yank)

(defun clean-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer)
)
(global-set-key "\C-@" 'clean-buffer)

(defun osx-paste ()
  (interactive)
  (insert (shell-command-to-string "pbpaste"))
)
(global-set-key "\M-v" 'osx-paste)
