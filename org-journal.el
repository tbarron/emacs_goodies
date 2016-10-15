(provide 'org-journal)

; -----------------------------------------------------------------------------
; org-journal -- home grown org-mode related stuff
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; ~/Dropbox/journal/thisyear should be a symlink pointing at the
; current year directory in ~/Dropbox/journal (e.g.,
; ~/Dropbox/journal/2014). If this is the case, do nothing. If the
; symlink does not exist, create it pointing to the correct year
; directory. If the symlink points to a past year, delete it and
; recreate it pointing at the current year. This routine assumes that
; the current year directory exists.
; -----------------------------------------------------------------------------
(defun org-jnl-dropbox-update-year-symlink ()
  (setq org-journal-dir "~/Dropbox/journal")
  (let (
        (jnldir org-journal-dir)
        (cyr (format-time-string "%Y"))
       )
    (let (
          (fn  (concat jnldir "/thisyear"))
          (jnlcyr (concat jnldir "/" cyr))
          (tn))

      (if (not (file-exists-p jnlcyr))
          (make-directory jnlcyr)
      )
      (if (file-symlink-p fn)
          (progn
            (setq tn (file-name-nondirectory (file-truename fn)))
            (message "'%s' => '%s'" fn tn)
            (if (not (equal tn cyr))
                (progn
                  (delete-file fn)
                  (make-symbolic-link (format-time-string "%Y") fn)
                  (message "'%s' ~= '%s' -- removed and remade symlink %s"
                           tn cyr fn)
                )
            )
          )
          (message "symlink %s not found -- making it" fn)
          (make-symbolic-link (format-time-string "%Y") fn)
      )
    )
  )
)
; (debug-on-entry 'org-jnl-dropbox-update-year-symlink)

; -----------------------------------------------------------------------------
(defun org-jnl-open ()
  "Access journal files"
  (interactive)

  (let ((diary_jpy (format-time-string "~/diary/journal/personal/%Y"))
        (diary_jw "~/diary/journal/work")
        (diary_jy (format-time-string "/journal/%Y"))
        (diary_jwy (format-time-string "~/diary/journal/work/%Y"))
        (diary_jwym (format-time-string "~/diary/journal/work/%Y/%m"))
        (prj_worktime "~/prj/worktime")
        ; (jnl "~/Dropbox/journal")
        ; (jnl org-journal-dir)
        (jnl_year (concat org-journal-dir (format-time-string "/%Y")))
        (thumb "/Volumes/ZAPHOD")

;;         (daily (format-time-string "%d.dodo"))
;;         (journal (format-time-string "%m.txt"))
;;         (queue "queue.dodo")
;;         (stats "FX.txt")
;;         (projects "PROJECTS.txt")
;;         (workfile (format-time-string "%Y.txt"))
;;         (worklog (format-time-string "%Y.txt"))
;;         (worklog "WORKLOG")
        )

    ; (do-findfile "~/Dropbox/journal" "work.do")
    (do-findfile org-journal-dir "work.do")
    ; (do-findfile "~/Dropbox/journal" "personal.do")
    (do-findfile org-journal-dir "personal.do")
    (do-findfile jnl_year "work.jnl")
    (goto-char (point-max))
    (do-findfile jnl_year "personal.jnl")
    (goto-char (point-max))
    (do-findfile jnl_year "WORKLOG")
    (goto-char (point-max))
  )
)

; ---------------------------------------------------------------------------
(defun org-archive-subtree-done ()
  "complete a TODO item and archive it in one step"
  (interactive)
  (org-todo 'done)
  (org-archive-subtree)
)

; ---------------------------------------------------------------------------
(defun org-move-subtree-end ()
  "Move the current subtree to the end of the file"
  (interactive)
  (save-excursion
    (org-cut-subtree)
    (goto-char (point-max))
    (yank)
  )
)

; ---------------------------------------------------------------------------
(defun org-first-entry ()
  "Return the position of the first top level headline in the file"
  (interactive)
  (let ( (rval) )
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^\* ")
      (setq rval (- (point) 2))
    )
    rval
  )
)

; ---------------------------------------------------------------------------
(defun show-org-first-entry ()
  (interactive)
  (message "%d" (org-first-entry))
)

; ---------------------------------------------------------------------------
(defun org-move-subtree-top ()
  "Move the current subtree to the top of the file"
  (interactive)
  (let ( (size)
       )
    (save-excursion
      (org-back-to-heading)
      (org-mark-subtree)
      (setq size (- (region-end) (region-beginning)))
      (org-cut-subtree)
      (goto-char (org-first-entry))
      (yank)
    )
  )
)

; ---------------------------------------------------------------------------
(defun org-jnl-now ()
  "Insert a time stamp for the current date and time"
  (interactive)

  (insert (format-time-string "\n* %Y-%m-%d %a %H:%M\n\n  "))
)

; ---------------------------------------------------------------------------
(defun org-jnl-new-entry ()
  "Insert a new TODO entry in a .do file or a current level timestamped
headline in a .jnl file"
  (interactive)

  (let ((ftype
         (message "%s" (car (last (split-string (buffer-file-name) "\\." t)))))
       )
    (if (equal ftype "jnl")
        (progn
          (goto-char (point-max))
          (org-insert-heading)
          (insert (format-time-string "%Y-%m-%d %a %H:%M\n\n  "))
        )
      (if (equal ftype "do")
          (org-jnl-new-do-entry)
      )
    )
  )
)

; ---------------------------------------------------------------------------
(defun org-jnl-new-do-entry ()
  "Insert a new TODO entry. If point is before the first TODO, insert just before the first one. If point is in a TODO entry, insert just before the current entry."
  (interactive)
  (let ((first-pos (org-first-entry))
       )
    (message "%d ? %d" (point) first-pos)
    (if (< (point) first-pos)
        (goto-char first-pos)
        (if (/= (point) (point-max))
            (org-forward-same-level 1)
        )
    )
    (org-insert-todo-heading t)
  )
)

; ---------------------------------------------------------------------------
(defun org-jnl-new-do-entry2 ()
  "Insert a new TODO entry. If point is before the first TODO, insert just before the first one. If point is in a TODO entry, insert just before the current entry."
  (interactive)
  (let ((first-pos (org-first-entry))
       )
    (message "%d ? %d" (point) first-pos)
    (if (< (point) first-pos)
        (goto-char first-pos)
        (if (/= (point) (point-max))
            (org-forward-same-level 1)
        )
    )
    (if (org-at-item-checkbox-p)
        (org-insert-item t)  ; add a checkbox item
      (if (org-at-item-p)
          (org-insert-item)  ; add the same kind of item as current one
        (progn
          (org-forward-same-level 1)   ; add a TODO headline after the
          (org-insert-todo-heading t)  ; current one
        )
      )
    )
  )
)

; ---------------------------------------------------------------------------
(defun org-jnl-new-item ()
  "Call org-list-insert-item"
  (interactive)
  (org-list-insert-item)
)

; ---------------------------------------------------------------------------
(defun org-jnl-insert-checkbox ()
  "Insert a checkbox before the item where point is"
  (interactive)
  (if (org-at-item-checkbox-p)
      (progn
        (end-of-line)
        (org-insert-item t)
        ; (insert "\n")
        (org-beginning-of-item)
        (end-of-line)
        )
    (progn
      (org-next-item)
      (insert "- [ ] \n")
      (org-indent-item)
      (org-end-of-item)
      )
    )
)
; (debug-on-entry 'org-jnl-insert-checkbox)

; ---------------------------------------------------------------------------
(global-set-key "\C-x?" 'org-jnl-open)
(global-set-key "\M-?" 'org-jnl-open)
(global-set-key "\C-c." 'org-jnl-now)

; ---------------------------------------------------------------------------
(setq org-mode-hook
      '(lambda ()
         (local-set-key "\M-k" 'org-move-subtree-up)
         (local-set-key "\M-j" 'org-move-subtree-down)
         (local-set-key "\M-i" 'org-jnl-new-item)
         (local-set-key "\M-[" 'org-promote-subtree)
         (local-set-key "\M-]" 'org-demote-subtree)
         (local-set-key "\M-^" 'org-move-subtree-top)
         (local-set-key "\M-$" 'org-move-subtree-end)
         (local-set-key "\C-a" 'backward-word)
         (local-set-key "\C-ca" 'org-agenda)
         (local-set-key "\M-d" 'org-todo)
         (local-set-key "\M-z" 'org-archive-subtree-done)
         (local-set-key "\M-e" 'org-jnl-new-entry)
         (local-set-key "\M-t" 'org-jnl-new-do-entry2)
         (local-set-key "\M-h" 'help-command)
         ; (local-set-key "\M-." 'org-mark-element)
         (local-set-key "\M-." 'org-jnl-insert-checkbox)
         (local-set-key (kbd "<left>") 'org-timestamp-down)
         (local-set-key (kbd "<right>") 'org-timestamp-up)
         (setq fill-column 79)
         ))


; ---------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.done$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.[dD][oO]$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.jnl$" . org-mode))

;; This routine checks the symlink thisyear against the current year
;; in org-journal-dir and updates it if it's out of date
(org-jnl-dropbox-update-year-symlink)
