;;; ---------------------------------------------------------------
;;; diary-name
;;; 2001-07-29 - replaced call to date with format-time-string
;;;              fixed bug that created dir $HOME/info/diary/2
;;; 2001-11-23 - rather than mucking around in a buffer, just use strings
;;;              moved journal location
;;;              split journal into personal and work sections
;;; 2001-11-24 - replaced (call-process "mkdir") with (make-directory)
;;; ---------------------------------------------------------------
(defun diary-name (which)
  (setq d_dir (concat (getenv "HOME")
                      "/info/diary/journal/"
                      which
                      (format-time-string "/%Y")))
  (if (not (file-directory-p d_dir))
      (make-directory d_dir 't)
    )
  (concat d_dir "/" (format-time-string "%m"))
)

;;; ---------------------------------------------------------------
;;; diary-issues
;;; ---------------------------------------------------------------
(defun diary-issues ()
  "Load the issues file in a dired buffer"
  (interactive)
  (dired (concat (getenv "HOME") "/info/diary/issues/*.txt"))
)

;;; ---------------------------------------------------------------
;;; diary
;;; 2001-07-29 - call diary-name rather than inlining its code
;;; 2001-11-23 - replaced (end-of-buffer) with (goto-char (point-max))
;;;              split journal into personal and work sections
;;; ---------------------------------------------------------------
(defun diary ()
  "Access today's personal or work diary file"
  (interactive)
  (setq which (read-string "work|personal? [w] > "))
  (if (string= which "") (setq which "work") 
    (if (string= which "w") (setq which "work")
      (setq which "personal")))
  (find-file (diary-name which))
  (text-mode)
)

;;; ---------------------------------------------------------------
;;; diary-append-entry
;;; ---------------------------------------------------------------
(defun diary-append-entry ()
  "Add an entry to today's personal or work diary file"
  (interactive)
  (diary)
  (append-entry "../../template")
;  (if (equal (point-max) 1)
;      (insert "                                  Updated: < >\n"))
;  (goto-char (point-max))
;  (insert "\n--- ")
;  (dt-time)
;  (insert " ---\n\n   ")
;  (text-mode)
;  (setq fill-prefix "   ")
)

;;; ---------------------------------------------------------------
;;; dt-format
;;; 2008.1030 - prompt for and set dt-format
;;; ---------------------------------------------------------------
(defun dt-format (fmt)
  "Prompt for and set dt-format"
  (let ((prompt "date format > "))
    (if (null fmt)
        (progn 
          (if (null dt-format-str)
              (setq dt-format-str "%Y.%m%d"))
          (setq dt-format-str (read-string prompt dt-format-str)))
      (setq dt-format-str (read-string prompt fmt))))
)

;;; ---------------------------------------------------------------
;;; dt-date
;;; 97-08-17 - got rid of "date" call using current-time and 
;;;            format-time-string
;;; 2008.1030 - make this a generalized time stamp function
;;;             prompt for offset -- n(ow), y(esterday), t(omorrow)
;;;             and format
;;;
;;;             format persists from one call to the next, defaults
;;;             to %Y.%m%d if nil
;;; ---------------------------------------------------------------
(defun dt-date ()
  "Insert a timestamp into the current buffer"
  (interactive)
  (defvar dt-format-str "%Y.%m%d" "The persistent timestamp format")
  (let (offset)
    (setq offset 
          (read-string "Offset [y(esterday), n(ow), t(omorrow)] > " "n"))
    ; (message "offset = %s; format = %s" offset format)
    (if (string-equal offset "n")
        (setq when (current-time))
      (if (string-equal offset "y")
          (setq when (time-subtract (current-time) (days-to-time 1)))
        (if (string-equal offset "t")
            (setq when (time-add (current-time) (days-to-time 1)))
          (message "invalid offset: %s" offset)
          )))

    (setq dt-format-str (read-string "Format > " dt-format-str))

    (insert (format-time-string dt-format-str when))
    )
)

;;; -----------------------------------------------------------------
;;; insert a date/time stamp plus a dividing line
;;; -----------------------------------------------------------------
(defun dt-date-divline ()
  "Insert a timestamp and divider line into the current buffer"
  (interactive)
  (goto-char (point-max))
  (if (re-search-backward "^[0-9]+-" 0 't)
      (progn (set 'd-bufday (substring (buffer-string) 
                                (- (point) 1)
                                (+ (point) 9)))
             (set 'd-today (format-time-string "%Y-%m-%d"))
             (if (string-equal d-bufday d-today)
                 (setq d-div
                       " --------------------------------------------------")
               (setq d-div
                     " =================================================="))
             (goto-char (- (point-max) 2))
             
             (if (looking-at "\n\n")
                 (goto-char (point-max))
               (if (looking-at ".\n")
                   (progn (goto-char (point-max))
                          (insert "\n"))
                 (progn (goto-char (point-max))
                        (insert "\n\n"))
                 ))
  
             (insert (format-time-string "%Y-%m-%d %H:%M:%S") d-div "\n\n   ")
             )

    (progn (goto-char 0)
           (setq d-div " ==================================================")
           (insert (format-time-string "%Y-%m-%d %H:%M:%S") d-div "\n\n   ")
           )
    )
)
(global-set-key "\C-x-" 'dt-date-divline)

;;; -----------------------------------------------------------------
(defun dt-check ()
 (interactive)
 (debug)
 (if (boundp 'dt-format-str)
   (message "dt-format-str = %s" dt-format-str)
     (message "argh! gnash gnash"))
)

;;; ---------------------------------------------------------------
;;; dt-yesterday
;;; 2008.1030 - insert yesterday's date and time into the buffer
;;; ---------------------------------------------------------------
(defun dt-yesterday ()
  "Insert yesterday's date into the current buffer using dt-format-str"
  (interactive)
  (insert (format-time-string 
           (dt-format 'nil)
           (time-subtract (current-time) (days-to-time 1))))
)

;;; ---------------------------------------------------------------
;;; dt-mdy
;;; 2005-08-01 - created
;;; ---------------------------------------------------------------
(defun dt-mdy (&optional dt-ovr)
  "Insert today's date (mm/dd/yy) into the current buffer"
  (interactive "P")
  (message "dt-ovr = " dt-ovr)
  (if (not (equal dt-ovr 'nil))
      (delete-char 8))
  (insert (format-time-string "%m/%d/%y" (current-time)))
)

;;; ---------------------------------------------------------------
;;; dt-datetime
;;; 2001-11-23 - insert the time string directly rather than using
;;;              a variable
;;; 2005-01-27 - renamed from dt-time
;;; ---------------------------------------------------------------
(defun dt-datetime ()
  "Insert a time and date stamp into the current buffer"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
)

;;; ---------------------------------------------------------------
;;; dt-time
;;; 2001-11-23 - insert the time string directly rather than using
;;;              a variable
;;; 2005-01-27 - new time only function
;;; ---------------------------------------------------------------
(defun dt-time ()
  "Insert a time and date stamp into the current buffer"
  (interactive)
  (insert (format-time-string (dt-format "%H:%M:%S")))
)

;;; ---------------------------------------------------------------
;;; newline-and-indent-relative
;;; ---------------------------------------------------------------
(defun newline-and-indent-relative ()
  "Add a new line and indent relative to the preceding line"
  (interactive)
  (newline)
  (indent-relative)
)

;;; ---------------------------------------------------------------
;;; move current line to top of screen
;;; ---------------------------------------------------------------
(defun tos ()
  "Move the current line to the top of the screen"
  (interactive)
  (recenter 0)
)

;;; ---------------------------------------------------------------
;;; Insert numbered lines the current buffer
;;; ---------------------------------------------------------------
(defun number (start stop step)
  "Insert numbered lines into the current buffer"
  (interactive "nStart: \nnStop: \nnStep: ")
  (message (concat (int-to-string start) " "
                   (int-to-string stop) " "
                   (int-to-string step)))
  (setq num start)
  (while (<= num stop)
    (insert (int-to-string num))
    (insert "\n")
    (setq num (+ num step))
  )
)

;;; ---------------------------------------------------------------
;;; Set a bookmark
;;; ---------------------------------------------------------------
(defun bookmark ()
  "Set a bookmark"
  (interactive)
  (setq bm-point (point))
  (setq bm-buffer (buffer-name))
  (find-file (concat (buffer-name) ".bkmk"))
  (erase-buffer)
  (insert (int-to-string bm-point))
  (save-buffer)
  (find-file bm-buffer)
)

;;; ---------------------------------------------------------------
;;; Goto a bookmark
;;; ---------------------------------------------------------------
(defun goto-bookmark ()
  "Go to the last set bookmark"
  (interactive)
  (setq bm-buffer (buffer-name))
  (find-file (concat (buffer-name) ".bkmk"))
  (message (buffer-string))
  (setq bm-point (string-to-int (buffer-string)))
  (find-file bm-buffer)
  (goto-char bm-point)
)

;;; ---------------------------------------------------------------
;;; Consult the phonebook
;;; ---------------------------------------------------------------
(defun phone ()
  "Check the user's and system phone book"
  (interactive)
  (switch-to-buffer (get-buffer-create "**phone**"))
  (erase-buffer)
  (setq match-s (read-minibuffer "string to match: "))
  (setq match-s (format "%s" match-s))
  (call-process "/local/bin/phone" nil t nil "" match-s)
)

;;; ---------------------------------------------------------------
;;; Search my diary for specified strings
;;; ---------------------------------------------------------------
(defun diary-search ()
  "Search my diary for a specified string"
  (interactive)
  (switch-to-buffer (get-buffer-create "*diary-search*"))
  (setq search-string (read-string "Diary search string: "))
  (call-process "find" nil "*diary-search*" 't 
                "/acct/tb/info/diary" "-name" "\?\?" "-exec"
                "grep" "-i" search-string "{}" "/dev/null" "\;")
  (insert "\n* * * Search completed * * *\n")
)

;;; ---------------------------------------------------------------
; visit the defects log file
;;; ---------------------------------------------------------------
(defun diary-visit-defects ()
  "visit the defects log file"
  (interactive)
  (find-file (concat (getenv "HOME") "/info/diary/defects/defects.log"))
)

;;; ---------------------------------------------------------------
; create a new issue file and instantiate the issue template
;;; ---------------------------------------------------------------
(defun new-issue ()
  "create a new issue"
  (interactive)

  ; construct paths for the issue file and template file
  (setq ni-dir (concat (getenv "HOME") "/info/diary/issues/"))
  (setq ni-filename 
        (concat (format-time-string "%Y-%m-%d.")
                (read-string "Issue name? > ")
                ".txt"))
  (setq ni-pathname (concat ni-dir ni-filename))
  (setq ni-template (concat (getenv "HOME") "/info/templates/issue"))

  ; open/create the issue file
  (find-file ni-pathname)

  ; insert the template
  (insert-file ni-template)

  ; at the end of the first line, insert the current date/time
  (end-of-line)
  (dt-time)
  (forward-line 1)
  (end-of-line)
)

;;; ---------------------------------------------------------------
; append a template to the current file
;;;; ---------------------------------------------------------------
(defun append-entry-interactive ()
  "append an entry to the end of the current file"
  (interactive)
  (setq parents 0)
  (setq default-path "template")
  (while (and (not (file-readable-p default-path)) (< parents 10))
    (progn (setq default-path (concat "../" default-path))
           (setq parents (+ parents 1))))
  (if (file-readable-p default-path)
      (append-entry default-path)
    (message "no readable 'template' found in tree"))
)

;;; ---------------------------------------------------------------
; append a template to the current file
;;; ---------------------------------------------------------------
(defun append-entry (template-path)
  "add an to a structured file"

  (goto-char (point-max))
  (if (< 1 (point-max))
      (if (not (string= (buffer-substring (- (point-max) 1) (point-max)) "\n"))
          (insert "\n")))
  (delete-blank-lines)
  (if (string= template-path "") (setq template-path "template"))
  (insert-file-contents template-path)
  (setq ap-anchor (point))

  (re-search-forward "@")
  (setq ap-start (point))
  (re-search-forward "@")
  (forward-char -1)
  (setq ap-info (buffer-substring ap-start (point)))
  (delete-region (- ap-start 1) (+ (point) 1))

  (if (string-match "^%" ap-info)
      (insert (format-time-string ap-info)))

  (goto-char ap-anchor)
  (search-forward "(+)")
  (delete-char -3)
)
