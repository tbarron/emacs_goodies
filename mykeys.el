;;;; ===============================================================
;;;; Key bindings
;;;; ===============================================================
(global-set-key "\M-\\" 'indent-region)
(global-set-key "\C-y" 'clipboard-yank)
(defun dtw-and-save ()
  "Delete trailing whitespace then save the file"
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer)
)
(global-set-key "\C-@" 'dtw-and-save)
