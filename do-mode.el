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
  (define-key do-mode-map "\ed" 'do-pdone)
  (define-key do-mode-map "\ez" 'do-xdone)
  (define-key do-mode-map "\e<" 'do-odone)
  (define-key do-mode-map "\ei" 'do-into)
  (define-key do-mode-map "\es" 'do-sub-entry)
  (define-key do-mode-map "\ee" 'do-new-entry)
;  (define-key do-mode-map "\ex" 'do-cut-entry)
  (define-key do-mode-map "\ea" 'do-copy-entry)
  (define-key do-mode-map "\ep" 'do-yank-entry)
  (define-key do-mode-map "\e^" 'do-entry-to-top)
  (define-key do-mode-map "\e!" 'do-entry-order)
  (define-key do-mode-map "\e@" 'do-entry-after)
  (define-key do-mode-map "\e$" 'do-entry-to-end)
;  (define-key do-mode-map "\C-x\C-n" 'next-dodo)
;  (define-key do-mode-map "\C-x\C-p" 'do-previous-entry)
)

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

;;; ---------------------------------------------------------------------------
(defun previous-dodo ()
  "Find the previous DODO buffer"
  (interactive)
  (setq blist (reverse (buffer-list)))
  (while (not (string-match "DODO" (buffer-name (car blist))))
    (setq blist (cdr blist)))
  (switch-to-buffer (car blist))
  )
(global-set-key "\C-x\C-p" 'previous-dodo)

;;; ---------------------------------------------------------------------------
(defun next-dodo ()
  "Find the next DODO buffer"
  (interactive)
  (if (string-match "DODO" (buffer-name))
      (bury-buffer))
  (setq blist (buffer-list))
  (while (not (string-match "DODO" (buffer-name (car blist))))
    (setq blist (cdr blist)))

  (switch-to-buffer (car blist))
  )
(global-set-key "\C-x\C-n" 'next-dodo)

;;; ---------------------------------------------------------------
;;; do-done
;;; ---------------------------------------------------------------
(defun do-done (mark)
  "Move a task to the DONE section and mark it with MARK"
  (interactive)
  (let ((rgx "^ [-+.>^<x] ")
        (donesect "^--- DONE ---")
        (point-str (buffer-substring (point) (+ 3 (point))))
        (initial-point (point))
        (start 0)
        (end 0))
    (save-excursion
      (if (string-match rgx point-str)
          (forward-word)
        (if (string-match "[^ \n]{1,3}.{1,3}" point-str)
            (backward-word)
          (if (string-match ".{1,2}\n{1,2}" point-str)
              (backward-word)
          )
        )
      )

      (if (re-search-forward rgx nil 't)  ; find beginning of next entry
          (setq end_t (re-search-backward rgx))
        (setq end_t (point-max)))
      (goto-char initial-point)         ; back up to where we started
      (re-search-forward donesect)      ; find beginning of DONE section
      (setq end_d (re-search-backward donesect))
      (setq end (min end_t end_d))      ; take the min of the two
      (goto-char end)                   ; reposition to end of entry
      (setq start (re-search-backward rgx)) ; find beginning of target entry
      (kill-region start end)           ; and yank it to kill ring

      (re-search-forward donesect)      ; first entry in DONE section (or end of buf)
      (if (re-search-forward rgx (point-max) 'foobar)
          (re-search-backward rgx)
        )
      (if (not (eq ?\n (char-before)))
          (insert "\n"))

      (yank)                            ; yank the entry being moved
      (re-search-backward rgx)          ; back to the beginning of entry
      ;;(replace-match " + ")             ; replace the marker
      (replace-match mark)              ; replace the marker
      (save-buffer)                     ; and write the file
      )
    )
  )
(cancel-debug-on-entry 'do-done)

;;; ---------------------------------------------------------------
;;; do-pdone - mark entry as completed
;;; ---------------------------------------------------------------
(defun do-pdone ()
  "Mark a dodo entry as done (+) and move it to the DONE section"
  (interactive)
  (do-done " + ")
)

;;; ---------------------------------------------------------------
;;; do-xdone - mark entry as NOT done/abandoned (x)
;;; ---------------------------------------------------------------
(defun do-xdone ()
  "Mark a dodo entry as not done (x) and move it to the DONE section"
  (interactive)
  (do-done " x ")
)

;;; ---------------------------------------------------------------
;;; do-odone - mark entry as diverted
;;; ---------------------------------------------------------------
(defun do-odone ()
  "Mark a dodo entry as diverted (<) and move it to the DONE seciton"
  (interactive)
  (do-done " < ")
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
  (setq back-to (point))

  ; insert the entry after the "=== DONE ===" line
  (if (search-forward "=== DONE ===" (point-max) 't)
      (progn
        (end-of-line)
        (if (eq (point) (point-max))
            (insert "\n")
          (forward-char 1)
          )
        )
    (progn
      (goto-char (point-max))
      (insert "\n\n- === DONE ===============================================\n")
      )
    )
  (insert entry)
  (save-buffer)
  (goto-char back-to)
)

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
  (let ((donesect "^--- DONE ---")
        (initial-point (point))
        (initial-fill-prefix fill-prefix)
        (rgx "^ [-+.>^<x] ")
        )
    (if (re-search-forward rgx nil 't)
        (re-search-backward rgx))
    (setq fill-prefix "")
    (open-line 2)
    (insert " - [" (format-time-string "%Y.%m%d") "] ")
    (setq fill-prefix initial-fill-prefix)
    )


  ;; (setq search-exp "^ [-+.>^<x] ")
  ;; (if (not (equal (point) (point-max)))
  ;;     (forward-char 1))
  ;; (if (re-search-forward search-exp nil 'x)
  ;;     (progn (forward-char -1)
  ;;            ; (set-fill-prefix "!")
  ;;            (setq fill-prefix "")
  ;;            (open-line 2)
  ;;            (do-create-entry)
  ;;            )
  ;;   )
  ;; (if (eobp)
  ;;     (progn ; (set-fill-prefix "!")
  ;;            (setq fill-prefix "")
  ;;            (if (not (equal (char-after (- (point) 1)) 10))
  ;;                (open-line 2)
  ;;              )
  ;;            (if (not (equal (char-after (- (point-max) 2)) 10))
  ;;                (open-line 1)
  ;;              )
  ;;            (goto-char (point-max))
  ;;            (do-create-entry)
  ;;            )
  ;;   )
)

(defun do-create-entry ()
  (insert " - [" (format-time-string "%Y.%m%d") "] ")
  ; (dt-date)
  ; (insert "] ")
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
  (re-search-backward "[^ 	\n]")
  (forward-char 1)
  (newline)
  (insert "   > ")
  (insert (format-time-string "%Y.%m%d.%H%M" (current-time)))
  ; (dt-datetime)
  ; (forward-char -3)
  ; (kill-line)
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

;;; ---------------------------------------------------------------
;;; if date has changed, call add-reminders function
;;; ---------------------------------------------------------------
(defun do-remind ()
  "Check the update time of the DODO file and add appropriate reminders"
  (interactive)
  (setq mtime (nth 5 (file-attributes "~/Dropbox/DODO")))
  (setq mdays (time-to-days mtime))
  (setq ndays (time-to-days (current-time)))
  (message "mdays = %d; ndays = %d" mdays ndays)

  (if (not (= mdays ndays))
      (add-reminders)
    )
)

;;; ---------------------------------------------------------------
;;; add reminders
;;; ---------------------------------------------------------------
(defun add-reminders ()
  "Copy appropriate reminders from reminder file to DODO"
  (interactive)
)

;;; ---------------------------------------------------------------
;;; vvv boneyard vvv

;(defun non-saved-text-mode ()
;  "Like text-mode, but delete auto save file when file is saved for real."
;  (text-mode)
;  (make-local-variable 'delete-auto-save-files)
;  (setq delete-auto-save-files t))

; (setq do-DODO-FILE-NAME "~/info/diary/dodo/dodo")

;;; ---------------------------------------------------------------
;;; dodo
;;; ---------------------------------------------------------------
;; (defun dodo ()
;;   "Access diary files"
;;   (interactive)
;;
;;   (let ((diary_jpy (format-time-string "~/diary/journal/personal/%Y"))
;;         (diary_jw "~/diary/journal/work")
;;         (diary_jy (format-time-string "/journal/%Y"))
;;         (diary_jwy (format-time-string "~/diary/journal/work/%Y"))
;;         (diary_jwym (format-time-string "~/diary/journal/work/%Y/%m"))
;;         (prj_worktime "~/prj/worktime")
;;         (jnl_year (format-time-string "~/Dropbox/journal/%Y"))
;;         (thumb "/Volumes/ZAPHOD")
;;
;;         (daily (format-time-string "%d.dodo"))
;;         (journal (format-time-string "%m.txt"))
;;         (queue "queue.dodo")
;;         (stats "FX.txt")
;;         (projects "PROJECTS.txt")
;;         (workfile (format-time-string "%Y.txt"))
;;         (worklog (format-time-string "%Y.txt"))
;;         (worklog "WORKLOG")
;;         )
;;
;; ;    (if (file-exists-p "~/bin/makedodo")
;; ;        (call-process "~/bin/makedodo" nil "*Messages*" 't "-c" "30"))
;;
;;     ; (do-findfile diary_jpy stats)
;;     ; (do-findfile diary_jw projects)
;;     ; (do-findfile diary_jpy journal)
;;     ; (do-findfile diary_jwym daily)
;;     ; (do-findfile diary_jw queue)
;;     ; (do-findfile "~/Dropbox/journal" "work.do")
;;     ; (do-findfile "~/Dropbox/journal" "personal.do")
;;     ; (do-findfile jnl_year journal)
;;     (do-findfile jnl_year "WORKLOG")
;;     (goto-char (point-max))
;; ;;     (if (file-exists-p thumb)
;; ;;         (progn
;; ;;           ; (do-findfile (concat thumb "/prj") "ROOT.do")
;; ;;           ; (do-findfile (concat thumb diary_jy) journal)
;; ;;           ; (do-findfile (concat thumb diary_jy) worklog)
;; ;;           ; (goto-char (point-max))
;; ;;         )
;; ;;     )
;;   )
;; )
;; (global-set-key "\M-?" 'dodo)

;;; ---------------------------------------------------------------
;;; do-findfile
;;; ---------------------------------------------------------------
;; (defun do-findfile (path name)
;;   (if (not (file-directory-p path))
;;       (make-directory path)
;;       )
;;   (find-file (concat path "/" name))
;; )

;;; ---------------------------------------------------------------
;;; do-into
;;; ---------------------------------------------------------------
;; (defun do-into ()
;;   "Enter a file mentioned in the current line in the form \"(filename)\""
;;   (interactive)
;;   (beginning-of-line)
;;   (re-search-forward "-.*( *")
;;   (setq start (point-marker))
;;   (search-forward ".do")
;;   (find-file (buffer-substring start (point)))
;; ;  (text-mode)
;; )

;;; ---------------------------------------------------------------
;;; do-done-name
;;; 2001-11-24 - replaced (call-process "mkdir") with (make-directory)
;;; ---------------------------------------------------------------
;; (defun do-done-name ()
;;   "Return the name of today's done log file"
;;   (setq d_dir (concat (getenv "HOME")
;;                       (format-time-string "/info/diary/done/%Y/%m")))
;;   (if (not (file-directory-p d_dir))
;;       ; (call-process "mkdir" nil nil nil "-p" d_dir)
;;       (make-directory d_dir 't)
;;     )
;;   (concat d_dir "/" (format-time-string "%d.done"))
;; )

;;; ---------------------------------------------------------------
;;; do-done-name-test
;;; ---------------------------------------------------------------
;; (defun do-done-name-test ()
;;   "Test the function do-done-name"
;;   (interactive)
;;   (setq filename (do-done-name))
;;   (message "filename: '%s'" filename)
;; )

;;; ---------------------------------------------------------------
;;; do-visit-done
;;; ---------------------------------------------------------------
;; (defun do-visit-done ()
;;   "visit the done log"
;;   (interactive)
;;   (find-file (do-done-name))
;; )



;;; ^^^ boneyard ^^^
;;; ---------------------------------------------------------------


(message "file do-mode.el loaded")
