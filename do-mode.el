;; Do mode, and its ideosyncratic commands.

(defvar do-mode-syntax-table nil
  "Syntax table used while in do mode.")

(defvar do-mode-abbrev-table nil
  "Abbrev table used while in do mode.")
(define-abbrev-table 'do-mode-abbrev-table ())

(if do-mode-syntax-table
    ()
  (setq do-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " do-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " do-mode-syntax-table)
  (modify-syntax-entry ?' "w   " do-mode-syntax-table))

(defvar do-mode-map nil "")
(if do-mode-map
    ()
  (setq do-mode-map (make-sparse-keymap))
  (define-key do-mode-map "\t" 'tab-to-tab-stop)
  (define-key do-mode-map "\ed" 'do-done)
  (define-key do-mode-map "\ez" 'do-xdone)
  (define-key do-mode-map "\eo" 'do-odone)
  (define-key do-mode-map "\ei" 'do-into)
  (define-key do-mode-map "\es" 'do-sub-entry)
  (define-key do-mode-map "\ee" 'do-new-entry)
  (define-key do-mode-map "\ex" 'do-cut-entry)
  (define-key do-mode-map "\ea" 'do-copy-entry)
  (define-key do-mode-map "\ep" 'do-yank-entry)
  (define-key do-mode-map "\e^" 'do-entry-to-top)
  (define-key do-mode-map "\e!" 'do-entry-order)
  (define-key do-mode-map "\e@" 'do-entry-after)
  (define-key do-mode-map "\e$" 'do-entry-to-end)
  (define-key do-mode-map "\C-x\C-n" 'do-next-entry)
  (define-key do-mode-map "\C-x\C-p" 'do-previous-entry)
)


;(defun non-saved-text-mode ()
;  "Like text-mode, but delete auto save file when file is saved for real."
;  (text-mode)
;  (make-local-variable 'delete-auto-save-files)
;  (setq delete-auto-save-files t))

(setq do-DODO-FILE-NAME "~/info/diary/dodo/dodo")

(defun do-mode ()
  "Major mode for editing my todo files.  Special commands:\\{do-mode-map}
Turning on do-mode calls the value of the variable do-mode-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map do-mode-map)
  (setq mode-name "Dodo")
  (setq major-mode 'do-mode)
  (setq local-abbrev-table do-mode-abbrev-table)
  (set-syntax-table do-mode-syntax-table)
  (run-hooks 'do-mode-hook))

;;; ---------------------------------------------------------------
;;; dodo
;;; ---------------------------------------------------------------
(defun dodo ()
  "Access file do-DODO-FILE-NAME"
  (interactive)
  (find-file do-DODO-FILE-NAME)
  (re-search-forward "^-")
  (forward-char -1)
)

;;; ---------------------------------------------------------------
;;; do-into
;;; ---------------------------------------------------------------
(defun do-into ()
  "Enter a file mentioned in the current line in the form \"(filename)\""
  (interactive)
  (beginning-of-line)
  (re-search-forward "-.*( *")
  (setq start (point-marker))
  (search-forward ".do")
  (find-file (buffer-substring start (point)))
;  (text-mode)
)

;;; ---------------------------------------------------------------
;;; do-done-name
;;; 2001-11-24 - replaced (call-process "mkdir") with (make-directory)
;;; ---------------------------------------------------------------
(defun do-done-name ()
  "Return the name of today's done log file"
  (setq d_dir (concat (getenv "HOME")
                      (format-time-string "/info/diary/done/%Y/%m")))
  (if (not (file-directory-p d_dir))
      ; (call-process "mkdir" nil nil nil "-p" d_dir)
      (make-directory d_dir 't)
    )
  (concat d_dir "/" (format-time-string "%d.done"))
)

;;; ---------------------------------------------------------------
;;; do-done-name-test
;;; ---------------------------------------------------------------
(defun do-done-name-test ()
  "Test the function do-done-name"
  (interactive)
  (setq filename (do-done-name))
  (message "filename: '%s'" filename)
)

;;; ---------------------------------------------------------------
;;; do-visit-done
;;; ---------------------------------------------------------------
(defun do-visit-done ()
  "visit the done log"
  (interactive)
  (find-file (do-done-name))
)

;;; ---------------------------------------------------------------
;;; do-done
;;; ---------------------------------------------------------------
(defun do-done ()
  "Mark a dodo entry as done (+) and move it to today's diary file"
  (interactive)
  (do-mark-done "+")
)

;;; ---------------------------------------------------------------
;;; do-xdone - mark entry as NOT done (x)
;;; ---------------------------------------------------------------
(defun do-xdone ()
  "Mark a dodo entry as not done (x) and move it to today's diary file"
  (interactive)
  (do-mark-done "x")
)

;;; ---------------------------------------------------------------
;;; do-odone - mark entry as expanded or replaced (o)
;;; ---------------------------------------------------------------
(defun do-odone ()
  "Mark a dodo entry as expanded or replaced (o) and move it to the done log"
  (interactive)
  (do-mark-done "o")
)

;;; ---------------------------------------------------------------
;;; do-mark-done
;;; -- this function replicates the code of do-mark-current-entry
;;;    should be able to call do-mark-current-entry instead
;;; ---------------------------------------------------------------
(defun do-mark-done (do-mark)
  ; find the beginning of the task entry being completed
  (if (not (equal (point) (point-max)))
      (forward-char 1))
  (setq search-exp "^[-x><+]")
  (re-search-backward search-exp)
  (setq start (point-marker))

  ; start now points to the beginning of the entry
  (setq pchrs (char-to-string (char-after (point))))
  (if (or (string= pchrs "-")
	  (string= pchrs ">"))
      (progn (delete-char 1) (insert do-mark))
      (forward-char 1)
  )

  ; go to the end of the entry
  (if (re-search-forward search-exp nil 'x)
      (backward-char 1)
  )

  ; save the entry text as variable 'entry'
  (setq entry (buffer-substring start (point)))

  ; remove the entry from the current file
  (delete-region start (point))

  ; insert the entry into the done log file
  (setq xchr (substring entry 3 4))
  (if (and (string-lessp "/" xchr) (string-lessp xchr ":")) ; is a digit
      (progn
        (delete-other-windows)
        (split-window-vertically)
        (other-window 1)
        (find-file (do-done-name))                 ; in the done log
        (goto-char (point-max))
        (if (not (equal (point) 1))
            (progn (setq pchrs (char-to-string (char-after (- (point) 1))))
                   (if (not (string= pchrs "\n"))
                       (insert "\n\n")
                     )
                   )
          )
        ; (insert "--- ")
        ; (call-process "date" nil 't nil "+%A %y/%m/%d %H:%M:%S ---")
        (insert (format-time-string "--- %A %Y/%m/%d %H:%M:%S ---\n"))
        (insert entry)
        (save-buffer)
        (other-window 1)                       ; back to .do file
        ))
  (save-buffer)
)

;;;  ; find the beginning of the task entry being completed
;;;  (if (not (equal (point) (point-max)))
;;;      (forward-char 1))                  ; mark beginning of entry
;;;  (setq search-exp "^[-x><+]")
;;;  (re-search-backward search-exp)
;;;  (setq start (point-marker))
;;;
;;;  ; start now points to the beginning of the entry
;;;  (setq pchrs (char-to-string (char-after (point))))
;;;  (if (or (string= pchrs "-")
;;;	  (string= pchrs ">"))
;;;      (progn (delete-char 1) (insert "x"))
;;;      (forward-char 1)
;;;  )
;;;
;;;  ; go to the end of the entry
;;;  (if (re-search-forward search-exp nil 'x)
;;;      (backward-char 1)
;;;  )
;;;
;;;  ; save the entry text as variable 'entry'
;;;  (setq entry (buffer-substring start (point)))
;;;
;;;  ; remove the entry from the current file
;;;  (delete-region start (point))
;;;
;;;  ; insert the entry into the done log file
;;;  (setq xchr (substring entry 3 4))
;;;  (if (and (string-lessp "/" xchr) (string-lessp xchr ":")) ; is a digit
;;;      (progn
;;;        (delete-other-windows)
;;;        (split-window-vertically)
;;;        (other-window 1)
;;;        (find-file (do-done-name))                 ; in the diary file
;;;        (end-of-buffer)
;;;        (if (not (equal (point) 1))
;;;            (progn (setq pchrs (char-to-string (char-after (- (point) 1))))
;;;                   (if (not (string= pchrs "\n"))
;;;                       (insert "\n\n")
;;;                     )
;;;                   )
;;;          )
;;;        ; (insert "--- ")
;;;        ; (call-process "date" nil 't nil "+%A %y/%m/%d %H:%M:%S ---")
;;;        (insert (format-time-string "--- %A %Y/%m/%d %H:%M:%S ---\n"))
;;;        (insert entry)
;;;        (save-buffer)
;;;        (other-window 1)                       ; back to .do file
;;;        ))
;;;  (save-buffer)
;;;)

;;; ---------------------------------------------------------------
;;; do-today - obsolete - no longer used
;;; ---------------------------------------------------------------
(defun do-today ()
  "Move a dodo entry from a project-specific dodo file to today's dodo file"
  (interactive)
;  (diary-name)                           ; establish diary file name
  (if (not (equal (point) (point-max)))
      (forward-char 1))                  ; mark beginning of entry
  (setq search-exp "^[-x><+]")
  (re-search-backward search-exp)
  (setq start (point-marker))
  (setq pchrs (char-to-string (char-after (point))))
;  (if (or (string= pchrs "-")
;	  (string= pchrs ">"))
;      (progn (delete-char 1) (insert "+"))
;      (forward-char 1)
;  )
  (forward-char 1)
  (if (re-search-forward search-exp nil 'x)   ; go to end of entry
      (backward-char 1)
  )
  (setq entry (buffer-substring start (point)))
  (delete-region start (point))
  (other-window 1)
  (find-file "~/.dodo/today_have_to.do")
  (end-of-buffer)
  (if (not (equal (point) 1))
      (progn (setq pchrs (char-to-string (char-after (- (point) 1))))
             (if (not (string= pchrs "\n"))
                 (insert "\n\n")
             )
      )
  )
;  (insert "--- ")
;  (call-process "date" nil 't nil "+%A %H:%M:%S ---")
  (insert entry)
  (save-buffer)
  (other-window 1)                       ; back to .do file
  (save-buffer)
)

;;; ---------------------------------------------------------------
;;; do-cut-entry
;;; ---------------------------------------------------------------
(defun do-cut-entry ()
  "Cut the current entry to the kill ring for subsequent yanking"
  (interactive)
  (do-mark-current-entry)
  (setq entry (buffer-substring start (point)))
  (kill-region start (point))
)

;;; ---------------------------------------------------------------
;;; do-copy-entry
;;; ---------------------------------------------------------------
(defun do-copy-entry ()
  "Copy the current entry to the kill ring for subsequent yanking"
  (interactive)
  (do-mark-current-entry)
  (copy-region-as-kill start (point))
)

;;; ---------------------------------------------------------------
;;; do-yank-entry
;;; ---------------------------------------------------------------
(defun do-yank-entry ()
  "Yank the most recent kill from the kill ring before the current entry"
  (interactive)
  (do-beginning-of-entry)
  (yank)
)

;;; ---------------------------------------------------------------
;;; do-dated
;;; ---------------------------------------------------------------
(defun 
do-dated ()
  "Create a dated reminder entry based on dt parameters"
  (interactive)
  (setq dt-arg (read-from-minibuffer "dt argument> "))
  (set-buffer (get-buffer-create "*work*"))
  (erase-buffer)
  (call-process "/local/bin/dt" nil 't nil "~/.dodo/%y%m%d.dodo" dt-arg)
  (setq dt-fname (buffer-substring (point-min) (- (point-max) 1)))
  (find-file dt-fname)
)

;;;; ---------------------------------------------------------------
;;; do-new-entry
;;; ---------------------------------------------------------------
(defun do-new-entry ()
  "Create a new dodo entry with today's date"
  (interactive)
  (setq search-exp "^[-x><+]")
  (if (not (equal (point) (point-max)))
      (forward-char 1))
  (if (re-search-forward search-exp nil 'x)
      (progn (forward-char -1)
             ; (set-fill-prefix "!")
             (setq fill-prefix "")
             (open-line 2)
             (do-create-entry)
             )
    )
  (if (eobp)
      (progn ; (set-fill-prefix "!")
             (setq fill-prefix "")
             (if (not (equal (char-after (- (point) 1)) 10))
                 (open-line 2)
               )
             (if (not (equal (char-after (- (point-max) 2)) 10))
                 (open-line 1)
               )
             (goto-char (point-max))
             (do-create-entry)
             )
    )
)

(defun do-create-entry ()
  (insert "- [")
  (dt-date)
  (insert "] ")
  (setq fill-prefix "   ")
)

;;; ---------------------------------------------------------------
;;; do-sub-entry
;;; ---------------------------------------------------------------
(defun do-sub-entry ()
  "Create a sub-entry within the current dodo entry"
  (interactive)
  (setq search-exp "^[-x><+]")
  (forward-char 1)
  (if (re-search-forward search-exp nil 'x)
      (backward-char 1)
    )
  (re-search-backward "[]a-zA-Z.;,:0-9?()\"]")
  (forward-char 1)
  (newline)
  (insert "   > ")
  (dt-time)
  (forward-char -3)
  (kill-line)
  (insert ": ")
  (setq fill-prefix "      ")
)

;;; ---------------------------------------------------------------
;;; goto today's dodo file in the current buffer
;;; ---------------------------------------------------------------
(defun do-goto-today ()
  "Jump to today's dodo file"
  (interactive)
  (find-file "~/.dodo/today_have_to.do")
)

;;; ---------------------------------------------------------------
;;; bracket the current entry - set mark start at beginning and put
;;; point at the end
;;; ---------------------------------------------------------------
(defun do-mark-current-entry ()
  (if (not (equal (point) (point-max)))
      (forward-char 1))                  ; mark beginning of entry
  (setq search-exp "^[-x><+]")
  (re-search-backward search-exp)
  (setq start (point-marker))
  (forward-char 1)
  (if (re-search-forward search-exp nil 'x)   ; go to end of entry
      (backward-char 1)
  )
)

;;; ---------------------------------------------------------------
;;; go to the beginning of the current dodo entry
;;; ---------------------------------------------------------------
(defun do-beginning-of-entry ()
  (interactive)
  (if (not (equal (point) (point-max)))
      (forward-char 1))                  ; mark beginning of entry
  (setq search-exp "^[-x><+]")
  (re-search-backward search-exp)
)

;;; ---------------------------------------------------------------
;;; go to the beginning of the previous dodo entry
;;; ---------------------------------------------------------------
(defun do-previous-entry ()
  (interactive)
  (setq search-exp "^[-x><+]")
  (do-beginning-of-entry)
  (re-search-backward search-exp (point-min) 'not-t-not-nil)
)

;;; ---------------------------------------------------------------
;;; go to the beginning of the next dodo entry
;;; ---------------------------------------------------------------
(defun do-next-entry ()
  (interactive)
  (if (not (equal (point) (point-max)))
      (forward-char 1))                  ; mark beginning of entry
  (setq search-exp "^[-x><+]")
  (if (re-search-forward search-exp (point-max) 'not-t-not-nil)
      (forward-char -1))
  (do-beginning-of-entry)
)

;;; ---------------------------------------------------------------
;;; move the current entry to top of file
;;; ---------------------------------------------------------------
(defun do-entry-to-top ()
  "Move the current entry to the top of the file"
  (interactive)
  (setq there (point))
  (do-next-entry)
  (setq here (point))
  (if (> here there)
      (forward-char -1))
  (do-cut-entry)

  (goto-char (point-min))
  (yank)

  (goto-char here)
  (forward-char -1)
  (do-beginning-of-entry)
)

;;; ---------------------------------------------------------------
;;; move the current entry to just before a line of "- =======..."
;;; ---------------------------------------------------------------
(defun do-entry-order ()
  "Move the current entry to precede the divider line ('- ====...')"
  (interactive)
  (setq there (point))
  (do-next-entry)
  (setq here (point))
  (if (> here there)
      (forward-char -1))
  (do-cut-entry)

  (re-search-backward "^- ======")
  (yank)

  (goto-char here)
  (forward-char -1)
  (do-beginning-of-entry)
)

;;; ---------------------------------------------------------------
;;; move the current entry to just after a line of "- =======..."
;;; ---------------------------------------------------------------
(defun do-entry-after ()
  "Move the current entry to follow the divider line ('- ====...')"
  (interactive)
  (setq there (point))
  (do-next-entry)
  (setq here (point))
  (if (> here there)
      (forward-char -1))
  (do-cut-entry)

  (re-search-forward "^- ======")
  (do-next-entry)
  (yank)

  (goto-char here)
  (forward-char -1)
  (do-beginning-of-entry)
)

;;; ---------------------------------------------------------------
;;; move the current entry to bottom of file
;;; ---------------------------------------------------------------
(defun do-entry-to-end ()
  "Move the current entry to the top of the file"
  (interactive)
  (do-cut-entry)
  (setq here (point))
  (goto-char (point-max))
  (yank)
  (goto-char here)
;  (do-next-entry)
)

; (global-unset-key "\M-^")
; (global-unset-key "\M-$")

(global-set-key "\M-^" 'do-entry-to-top)
(global-set-key "\M-$" 'do-entry-to-end)
