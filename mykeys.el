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

(defun copy-comment-line ()
  "Copy the current line and comment the copy"
  (interactive)
  (save-excursion
    (let ((start) (end) (text))
      (beginning-of-line)
      (setq start (point))
      (forward-line 1)
      (setq end (point))
      (copy-region-as-kill start end)
      (yank)
      (setq end (point))
      (forward-line -1)
      (comment-region (point) end)
      )))
(global-set-key "\C-cc" 'copy-comment-line)

(defun copy-comment-region (start end)
  "Copy the marked region, insert it after a blank line, then
comment the copy."
  (interactive "*r")
  (save-excursion
    (if (not mark-active)
        (message "Please set the mark (\e-space) and try again.")
      (goto-char end)
      (insert "\n" (buffer-substring start end) "\n")
      (comment-region end (point)))
    ))
(global-set-key "\C-c\C-y" 'copy-comment-region)

(message "buffer mykeys.el loaded")
;; available key sequences
;; \C-c a
;; \C-c b
;; \C-c c
;; \C-c d
;; \C-c e
;; \C-c f
;; \C-c g
;; \C-c h

