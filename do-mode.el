;;; ---------------------------------------------------------------------------
;; do-mode supports tracking, editing, and managing lists of tasks
;; that can be checked off and moved to a "done" section of the file
;; when completed.
;;
;; status flag meaning
;;   +   working
;;   -   needs work
;;   ?   needs testing
;;
;;  keys  status  function          purpose
;;  \ee     +     do-new-task        create a new task
;;  \ed     +     do-pdone           mark task completed (+)
;;  \ez     +     do-xdone           mark task abandoned (x)
;;  \e/     +     do-odone           divert task (<) (i.e., moved elsewhere)
;;  \C-v    +     do-goto-next-task  jump to next task
;;  \C-t    +     do-goto-prev-task  jump to previous task
;;  \C-xj   -?    do-task-down       move current task down
;;  \C-xk   -?    do-task-up         move current task up
;;  \e^     -?    do-task-to-top     move current task to top
;;  \e$     -?    do-task-to-bot     move current task to bottom
;;  \em     -?    do-mark-task       bracket task with mark and point
;;  \C-c x  -?    do-kill-task       kill task into the kill-ring
;;
; (message "loading do-mode.el: 0%%")
(if (featurep 'do-mode)
    (unload-feature 'do-mode 't))

(provide 'do-mode)

;;; ---------------------------------------------------------------------------
(defvar do-mode-syntax-table nil
  "Syntax table used while in do mode.")

;;; ---------------------------------------------------------------------------
(if do-mode-syntax-table
    ()
  (setq do-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " do-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " do-mode-syntax-table)
  (modify-syntax-entry ?' "w   " do-mode-syntax-table))

;;; ---------------------------------------------------------------------------
(defvar do-mode-abbrev-table nil
  "Abbrev table used while in do mode.")

(define-abbrev-table 'do-mode-abbrev-table ())

;;; ---------------------------------------------------------------------------
;; defvar doesn't reset variables that are already defined. So we do a
;; little magic here to ensure that the keymap will be updated when
;; this file is reloaded. First, we create a keymap in local 'let'
;; variable map and set all our keystroke sequences in it. Then, if
;; do-mode-map is bound, we use setq to override whatever content it
;; has with map. If it is not bound, instead we call defvar to create
;; it and put the contents of map in it.
(let ((map (make-keymap)))
      (define-key map "\ee" 'do-new-task)
      (define-key map "\ed" 'do-pdone)
      (define-key map "\ez" 'do-xdone)
      (define-key map "\e/" 'do-odone)
      (define-key map "\C-c\C-r" 'reload-do-mode)
      (define-key map "\C-v" 'do-goto-next-task)
      (define-key map "\C-t" 'do-goto-prev-task)
      (if (boundp 'do-mode-map)
          (setq do-mode-map map)
        (defvar do-mode-map map
          "Define or reset the keystroke map for do-mode")))


;;; ---------------------------------------------------------------------------
(defconst do-mode-rgx-done "^--- DONE ---"
  "Regexp for finding the DONE line")

;;; ---------------------------------------------------------------------------
(defconst do-mode-rgx-task "^ [-+.>^<x] "
  "Regexp for finding a task marker")

;;; ---------------------------------------------------------------------------
(defconst do-mode-done-line (concat "\n--- DONE ------------------------------"
                                    "----------------------------------------")
  "Regexp for finding the DONE line")

;;; ===========================================================================
;;; Interactive, user-facing functions -- called directly from keyboard

;;; ---------------------------------------------------------------------------
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
(defun reload-do-mode ()
  "Reload this file and tests. After reloading files, re-establish do-mode."
  (interactive)
  (load-file "do-mode.el")
  (load-file "test-do-mode.el")
  (do-mode)
)

;;; ---------------------------------------------------------------------------
(defun previous-dodo ()
  "Jump to the previous do-mode buffer in the buffer list"
  (interactive)
  (setq blist (reverse (buffer-list)))
  (while (not (do-buffer-p (car blist)))
    (setq blist (cdr blist)))
  (switch-to-buffer (car blist))
  )
(global-set-key "\C-x\C-p" 'previous-dodo)

;;; ---------------------------------------------------------------------------
(defun next-dodo ()
  "Find the next DODO buffer"
  (interactive)
  (if (do-buffer-p) (bury-buffer))
  (setq blist (buffer-list))
  (while (not (do-buffer-p (car blist)))
    (setq blist (cdr blist)))
  (switch-to-buffer (car blist))
  )
(global-set-key "\C-x\C-n" 'next-dodo)

;;; ---------------------------------------------------------------
(defun do-new-task ()
  "Create a new dodo entry with today's date"
  (interactive)
  (let ((initial-fill-prefix fill-prefix)
        (prelines "")
        (done-pos)
        (task-pos)
        (where)
        )
    (delete-trailing-whitespace)
    (end-of-line)
    (setq done-pos (do-done-position))
    (setq task-pos (do-next-task-mark))
    (if (and (equal nil done-pos) (equal nil task-pos))
        (if (< 2 (setq where (point-max)))
            (if (not (string= "\n" (buffer-substring (- (point-max) 1) (point-max))))
                (setq prelines "\n\n")
              (setq prelines "\n")))
      (if (equal nil task-pos)
          (setq where done-pos)
        (if (equal nil done-pos)
            (setq where task-pos)
          (if (< task-pos done-pos)
              (setq where task-pos)
            (setq where done-pos)))))

    (setq fill-prefix "")
    (goto-char where)
    (insert prelines " - [" (format-time-string "%Y.%m%d") "] \n\n")
    (goto-char (- (point) 2))
    (setq fill-prefix initial-fill-prefix)))

;; ----------------------------------------------------------------------------
;; If point is on the last task mark in the file, we don't want to
;; move it
(defun do-goto-next-task ()
  "Move point to the next task mark"
  (interactive)
  (let ((target))
    (if (setq target (do-next-task-mark))
        (goto-char target)
      (point)
      )))

;; ----------------------------------------------------------------------------
;; If point is on the first task mark in the file, we don't want to
;; move it
(defun do-goto-prev-task ()
  "Move point to the preceding task mark"
  (interactive)
  (goto-char (do-prev-task-mark)))

;; ----------------------------------------------------------------------------
(defun do-pdone (&optional nosave)
  "Mark a dodo entry as done (+) and move it to the DONE section"
  (interactive)
  (let ((msg))
    (if (setq msg (do-done " + " nosave))
        (message msg))))

;; ----------------------------------------------------------------------------
(defun do-xdone (&optional nosave)
  "Mark a dodo entry as not done (x) and move it to the DONE section"
  (interactive)
  (let ((msg))
    (if (setq msg (do-done " x " nosave))
        (message msg))))

;; ----------------------------------------------------------------------------
(defun do-odone (&optional nosave)
  "Mark a dodo entry as diverted (<) and move it to the DONE seciton"
  (interactive)
  (let ((msg))
    (if (setq msg (do-done " < " nosave))
        (message msg))))


;;; ---------------------------------------------------------------
;; Helper functions - these get called indirectly by interactive
;; function to help get stuff done
;;; ---------------------------------------------------------------------------
(defun bytes-at (where count)
  "Return the next COUNT bytes at WHERE (adjusting for eobp)"
  (if (< where (point-min))
      (setq where (point-min)))
  (if (< (point-max) (+ where count))
      (setq count (- (point-max) where)))
  (buffer-substring (min where (- (point-max) count))
                    (min (+ count where) (point-max))))

;;; ---------------------------------------------------------------------------
(defun do-buffer-p (&optional which)
  "Return t if WHICH is a do buffer (has do-mode set or name ends with do-rgx"
  (let ((buf (if which
                 (with-current-buffer which
                   (current-buffer))
               (current-buffer))))
    (with-current-buffer buf
      (or (string= major-mode "do-mode")
          (string-match "[dD][oO]$" (buffer-name))))))

;;; ---------------------------------------------------------------------------
(defun do-add-done-iff ()
  "Insert a DONE line if there isn't one already present"
  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward do-mode-rgx-done nil 'limit))
        (insert do-mode-done-line))))

;; ----------------------------------------------------------------------------
(defun do-done (mark nosave)
  "Move a task to the DONE section and mark it with MARK"
  (interactive)
  (catch 'empty
    (if (< (point-max) 3)
        (throw 'empty "file too small to hold a task"))
    (let ((rgx "^ [-+.>^<x] ")
          (donesect "^--- DONE ---")
          ;; (point-str (buffer-substring (point) (+ 3 (point))))
          (point-str (bytes-at (point) 3))
          (initial-point (point))
          (start 0)
          (end 0)
          (text))
      (save-excursion
        (end-of-line)
        (if (re-search-forward rgx nil 't)  ; find beginning of next entry
            (setq end_t (re-search-backward rgx))
          ;; if the forward search above failed, point is after the last
          ;; task mark in the buffer.
          ;; if we search backward for a task mark and don't find one,
          ;; there aren't any tasks in the file and we're done, so return
          ;; if the backward search is successful, set end_t to (point-max)
          ;; so it will be greater than the done-sect position
          (if (re-search-backward rgx nil 't)
              (setq end_t (point-max))
            (throw 'empty "no tasks in file")))
        (goto-char initial-point)         ; back up to where we started

        (if (not (setq end_d (do-done-position)))
            ;; if do-done-position returns nil, there is no done line in
            ;; the buffer and we need to add one
            (progn (do-add-done-iff)
                   (setq end_d (do-done-position))))

        (setq end (min end_t end_d))      ; take the min of the two
        (goto-char end)                   ; reposition to end of entry

        (if (not (setq start (re-search-backward rgx nil 't)))
            ;; search failed, so no task to move -- give it up
            (throw 'empty "no active tasks found"))
        (setq text (buffer-substring start end)) ; save a copy
        (delete-region start end)                ; and delete it

        (re-search-forward donesect)      ; first entry in DONE section (or end of buf)
        (end-of-line)
        (if (re-search-forward rgx (point-max) 't)
            (re-search-backward rgx)
          (delete-trailing-whitespace)
          (insert "\n\n"))

        (insert text)                     ; insert the saved copy
        (re-search-backward rgx)          ; back to the beginning of entry
        (replace-match mark)              ; replace the marker
        (if (not nosave)
            (save-buffer)))
        )))

;; ----------------------------------------------------------------------------
(defun do-done-position ()
  (concat "Return the front of '^--- DONE ---' if present, or (point-max)"
          "if there's no DONE line")
  (let ((done-rgx "^--- DONE ---"))
    (save-excursion
      (goto-char (point-max))
      (re-search-backward done-rgx nil 't))))

;; ----------------------------------------------------------------------------
(defun do-next-task-mark ()
  "Return the position of the first task mark after point"
  (save-excursion
    (end-of-line)
    (if (re-search-forward do-mode-rgx-task nil 't)
        (re-search-backward do-mode-rgx-task))))

;; ----------------------------------------------------------------------------
(defun do-prev-task-mark ()
  "Return the position of the task that precedes point"
  (save-excursion
    (if (not (re-search-backward do-mode-rgx-task nil 't))
        (if (re-search-forward do-mode-rgx-task nil 't)
            (re-search-backward do-mode-rgx-task)))
    (point)))

;;; ---------------------------------------------------------------
;;; do-cut-entry
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-cut-entry ()
;;   "Cut the current entry to the kill ring for subsequent yanking"
;;   (interactive)
;;   (do-mark-current-entry)
;;   (setq entry (buffer-substring start (point)))
;;   (kill-region start (point))
;; )

;;; ---------------------------------------------------------------
;;; do-copy-entry
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-copy-entry ()
;;   "Copy the current entry to the kill ring for subsequent yanking"
;;   (interactive)
;;   (do-mark-current-entry)
;;   (copy-region-as-kill start (point))
;; )

;;; ---------------------------------------------------------------
;;; do-yank-entry
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-yank-entry ()
;;   "Yank the most recent kill from the kill ring before the current entry"
;;   (interactive)
;;   (do-beginning-of-entry)
;;   (yank)
;; )

;;; ---------------------------------------------------------------
;;; do-dated
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun
;; do-dated ()
;;   "Create a dated reminder entry based on dt parameters"
;;   (interactive)
;;   (setq dt-arg (read-from-minibuffer "dt argument> "))
;;   (set-buffer (get-buffer-create "*work*"))
;;   (erase-buffer)
;;   (call-process "/local/bin/dt" nil 't nil "~/.dodo/%y%m%d.dodo" dt-arg)
;;   (setq dt-fname (buffer-substring (point-min) (- (point-max) 1)))
;;   (find-file dt-fname)
;; )

;; !@! uncomment this
;; (defun do-create-entry ()
;;   (insert " - [" (format-time-string "%Y.%m%d") "] ")
;;   ; (dt-date)
;;   ; (insert "] ")
;;   (setq fill-prefix "   ")
;; )

;;; ---------------------------------------------------------------
;;; do-sub-entry
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-sub-entry ()
;;   "Create a sub-entry within the current dodo entry"
;;   (interactive)
;;   (setq search-exp "^[-x><+]")
;;   (forward-char 1)
;;   (if (re-search-forward search-exp nil 'x)
;;       (backward-char 1)
;;     )
;;   (re-search-backward "[^ 	\n]")
;;   (forward-char 1)
;;   (newline)
;;   (insert "   > ")
;;   (insert (format-time-string "%Y.%m%d.%H%M" (current-time)))
;;   ; (dt-datetime)
;;   ; (forward-char -3)
;;   ; (kill-line)
;;   (insert ": ")
;;   (setq fill-prefix "      ")
;; )

;;; ---------------------------------------------------------------
;;; goto today's dodo file in the current buffer
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-goto-today ()
;;   "Jump to today's dodo file"
;;   (interactive)
;;   (find-file "~/.dodo/today_have_to.do")
;; )

;;; ---------------------------------------------------------------
;;; bracket the current entry - set mark start at beginning and put
;;; point at the end
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-mark-current-entry ()
;;   (if (not (equal (point) (point-max)))
;;       (forward-char 1))                  ; mark beginning of entry
;;   (setq search-exp "^[-x><+]")
;;   (re-search-backward search-exp)
;;   (setq start (point-marker))
;;   (forward-char 1)
;;   (if (re-search-forward search-exp nil 'x)   ; go to end of entry
;;       (backward-char 1)
;;   )
;; )

;;; ---------------------------------------------------------------
;;; go to the beginning of the current dodo entry
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-beginning-of-entry ()
;;   (interactive)
;;   (if (not (equal (point) (point-max)))
;;       (forward-char 1))                  ; mark beginning of entry
;;   (setq search-exp "^[-x><+]")
;;   (re-search-backward search-exp)
;; )

;;; ---------------------------------------------------------------
;;; go to the beginning of the previous dodo entry
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-previous-entry ()
;;   (interactive)
;;   (setq search-exp "^[-x><+]")
;;   (do-beginning-of-entry)
;;   (re-search-backward search-exp (point-min) 'not-t-not-nil)
;; )

;;; ---------------------------------------------------------------
;;; go to the beginning of the next dodo entry
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-next-entry ()
;;   (interactive)
;;   (if (not (equal (point) (point-max)))
;;       (forward-char 1))                  ; mark beginning of entry
;;   (setq search-exp "^[-x><+]")
;;   (if (re-search-forward search-exp (point-max) 'not-t-not-nil)
;;       (forward-char -1))
;;   (do-beginning-of-entry)
;; )

;;; ---------------------------------------------------------------
;;; move the current entry to top of file
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-entry-to-top ()
;;   "Move the current entry to the top of the file"
;;   (interactive)
;;   (setq there (point))
;;   (do-next-entry)
;;   (setq here (point))
;;   (if (> here there)
;;       (forward-char -1))
;;   (do-cut-entry)
;;
;;   (goto-char (point-min))
;;   (yank)
;;
;;   (goto-char here)
;;   (forward-char -1)
;;   (do-beginning-of-entry)
;; )

;;; ---------------------------------------------------------------
;;; move the current entry to just before a line of "- =======..."
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-entry-order ()
;;   "Move the current entry to precede the divider line ('- ====...')"
;;   (interactive)
;;   (setq there (point))
;;   (do-next-entry)
;;   (setq here (point))
;;   (if (> here there)
;;       (forward-char -1))
;;   (do-cut-entry)
;;
;;   (re-search-backward "^- ======")
;;   (yank)
;;
;;   (goto-char here)
;;   (forward-char -1)
;;   (do-beginning-of-entry)
;; )

;;; ---------------------------------------------------------------
;;; move the current entry to just after a line of "- =======..."
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-entry-after ()
;;   "Move the current entry to follow the divider line ('- ====...')"
;;   (interactive)
;;   (setq there (point))
;;   (do-next-entry)
;;   (setq here (point))
;;   (if (> here there)
;;       (forward-char -1))
;;   (do-cut-entry)
;;
;;   (re-search-forward "^- ======")
;;   (do-next-entry)
;;   (yank)
;;
;;   (goto-char here)
;;   (forward-char -1)
;;   (do-beginning-of-entry)
;; )

;;; ---------------------------------------------------------------
;;; move the current entry to bottom of file
;;; ---------------------------------------------------------------
;; !@! uncomment this
;; (defun do-entry-to-end ()
;;   "Move the current entry to the top of the file"
;;   (interactive)
;;   (do-cut-entry)
;;   (setq here (point))
;;   (goto-char (point-max))
;;   (yank)
;;   (goto-char here)
;; ;  (do-next-entry)
;; )

; (global-unset-key "\M-^")
; (global-unset-key "\M-$")

;(global-set-key "\M-^" 'do-entry-to-top)
;(global-set-key "\M-^" 'do-entry-to-top)
;(global-set-key "\M-$" 'do-entry-to-end)

;;; ---------------------------------------------------------------
;;; if date has changed, call add-reminders function
;;; ---------------------------------------------------------------
; !@! uncomment this function
;; (defun do-remind ()
;;   "Check the update time of the DODO file and add appropriate reminders"
;;   (interactive)
;;   (setq mtime (nth 5 (file-attributes "~/Dropbox/DODO")))
;;   (setq mdays (time-to-days mtime))
;;   (setq ndays (time-to-days (current-time)))
;;   (message "mdays = %d; ndays = %d" mdays ndays)
;;
;;   (if (not (= mdays ndays))
;;       (add-reminders)
;;     )
;; )


;;; ---------------------------------------------------------------
;;; add reminders
;;; ---------------------------------------------------------------
; !@! uncomment this function
;; (defun add-reminders ()
;;   "Copy appropriate reminders from reminder file to DODO"
;;   (interactive)
;; )


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


(setq do-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      (setq tab-width 3)
      (setq fill-prefix "   ")))

(setq auto-mode-alist (cons (quote ("do$" . do-mode)) auto-mode-alist))

(message "loading do-mode.el: 100%%")
