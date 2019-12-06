;;;; ===============================================================
;;;; Key bindings
;;;; ===============================================================
(global-set-key "\C-c\C-f" 'forward-sexp)
(global-set-key "\C-c\C-b" 'backward-sexp)
(global-set-key "\C-x\C-c" 'save-buffers-kill-terminal)
(global-set-key "\M-b" 'eval-buffer)
(global-set-key "\M-\\" 'indent-region)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-c\C-a" 'indent-pp-sexp)

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

;; available key sequences
;; \C-c a
;; \C-c b
;; \C-c c
;; \C-c d
;; \C-c e
;; \C-c f
;; \C-c g
;; \C-c h

