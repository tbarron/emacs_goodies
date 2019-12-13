(setq rsf-exp "")

; ===========================================================================
; Insert the abbreviate hash of the current git commit at point
(defun githash ()
  "Insert the abbreviated hash of the current git commit"
  (interactive)
  (let ((tbuf-name "*user-temp*") (cbuf (current-buffer)) (hash nil))
    (shell-command "git log | head -1 | cut -c8-13" tbuf-name)
    (set-buffer tbuf-name)
    (setq hash (buffer-substring 1 (- (point-max) 1)))
    (set-buffer cbuf)
    (insert hash)
    )
  )

(global-set-key "\C-xh" 'githash)

; ===========================================================================
; set up a buffer for an inspection
(defun inspect ()
  "Make the buffer read-only and set tab-width to 8"
  (interactive)
  (toggle-read-only 1)
  (setq tab-width 8)
)

; ===========================================================================
; bracket quoted lines
(defun bracket-lines ()
  "Mark the beginning and end of a transcript or quote based on point & mark"
  (interactive)
  (setq bl-label (read-string "bracket label? > "))
  (setq bl-point (point))
  (setq bl-start (min (point) (mark)))
  (setq bl-end (max (point) (mark)))
  (goto-char bl-end)
  (insert "   ---- end " bl-label " ----\n")
  (goto-char bl-start)
  (insert "   --- begin " bl-label " ---\n")
  (goto-char bl-point)
)

; ===========================================================================
; repeatable search forward
(defun do-search-forward ()
  "Search forward, optionally repeating the last search"
  (interactive)
  (setq rsf-direction "forward")
  (if (null rsf-exp) (setq rsf-exp ""))
  (setq rsf-prompt (format "Search for? [%s] > " rsf-exp))
  (setq rsf-tmp (read-string rsf-prompt))
  (if (not (string-equal rsf-tmp "")) (setq rsf-exp (format "%s" rsf-tmp)) ())
  (re-search-forward rsf-exp)
)

; ===========================================================================
; repeatable search backward
(defun do-search-backward ()
  "Search backward, optionally repeating the last search"
  (interactive)
  (setq rsf-direction "backward")
  (if (null rsf-exp) (setq rsf-exp ""))
  (setq rsf-prompt (format "Search for? [%s] > " rsf-exp))
  (setq rsf-tmp (read-string rsf-prompt))
  (if (not (string-equal rsf-tmp "")) (setq rsf-exp (format "%s" rsf-tmp)) ())
  (re-search-backward rsf-exp)
)

; ===========================================================================
; repeat last search
(defun repeat-last-search ()
  "repeat the last search"
  (interactive)
  (message "searching %s for '%s'" rsf-direction rsf-exp)
  (if (string-equal rsf-direction "forward")
      (re-search-forward rsf-exp)
      (re-search-backward rsf-exp)
      )
)

; ===========================================================================
; reverse last search
(defun reverse-last-search ()
  "reverse the last search"
  (interactive)
  (if (string-equal rsf-direction "forward")
      (setq rsf-direction "backward")
      (setq rsf-direction "forward"))
  (message "searching %s for '%s'" rsf-direction rsf-exp)
  (if (string-equal rsf-direction "forward")
      (re-search-forward rsf-exp)
      (re-search-backward rsf-exp)
      )
)

; ===========================================================================
; double space from point to end of document
;
(defun double-space-to-end ()
  "Double-space the buffer from point to end"
  (interactive)
  (while (not (equal (point) (point-max)))
    (end-of-line)
    (insert "\n")
    (forward-line 1)
    )
)

; ===========================================================================
; single space from point to end of document
;
(defun single-space-to-end ()
  "Single-space the buffer from point to end"
  (interactive)
  (while (not (equal (point) (point-max)))
    (re-search-forward "^$")
    (delete-blank-lines)
    (forward-char 1)
    )
)

; ===========================================================================
; create a C function
;
(defun c-stub ()
  "Create a stub for a C function"
  (interactive)
  (setq fcn-name (read-from-minibuffer "Function name? > "))
  (insert-file "/home/tb/info/templates/fcn.c")
  (replace-string "@Name@" fcn-name)
  (forward-line -4)
  (back-to-indentation)
)

; ===========================================================================
; Open the file named under the cursor
;
(defun find-file-from-line ()
  "Use the blank delimited token under the cursor as a file name and visit the file"
  (interactive)
  (setq punct "[ -+=!@#$%^&*(){}?><,\|\n]")
  (setq start (point))
  (end-of-line)
  (setq eol (point))
  (beginning-of-line)
  (setq bol (point))

  (goto-char start)
  (if (re-search-backward punct bol 'limit)
      (forward-char 1))
  (setq there (point))
  (if (re-search-forward punct eol 'limit)
      (forward-char -1))
  (setq fname (buffer-substring (point) there))
  (goto-char there)
  (setq fname (read-from-minibuffer "Open file: " fname))
;  (message "fname = '%s'" fname)
  (find-file-other-window fname)
)
(global-set-key "\M-f" 'find-file-from-line)

; ===========================================================================
;
; Search for a PL/1 error in an error listing file and position on the
; associated error in the source code.
;
(defun find-pl1-err ()
  "Locate an error in PL1 source code"
  (interactive)

  (search-forward "(Line ")
  (setq here (point))
  (search-forward " ")
  (forward-char -1)
  (setq linenum (string-to-int (buffer-substring here (point))))
;  (message "line num: %d" linenum)

  (search-forward "file ")
  (setq here (point))
  (search-forward ")")
  (forward-char -1)
  (setq filename (downcase (buffer-substring here (point))))
  (tos)

  (message "going to line %d of file %s" linenum filename)

  (other-window 1)
  (find-file filename)
  (goto-char (point-min))
  (forward-line (- linenum 1))

  (other-window 1)
)

; ===========================================================================
;
; Kill all buffers whose name do NOT begin with "*"
;
(defun kill-all-user-buffers ()
  "Kill all user-created buffers"
  (interactive)
  (kill-matching-buffers "\*magit" 't)
  (setq foo (buffer-list))
  (while (not (equal nil foo))
    (setq bname (buffer-name (car foo)))
    (if (not (equal (substring bname 0 1) "*"))
        (kill-buffer bname)
      )
    (setq foo (cdr foo))
    )
)
(global-set-key "\C-x\C-k" 'kill-all-user-buffers)

; ===========================================================================
;
; Copy preceding line from point to end of line
;
(defun copy-preceding-line ()
  "Copy preceding line from point to end of line"
  (interactive)
  (setq here (point))
  (beginning-of-line)
  (setq offset (- here (point)))
  (forward-line -1)
  (forward-char offset)
  (setq beg (point))
  (end-of-line)
  (setq end (point))
  (setq str (buffer-substring beg end))
  (goto-char here)
  (insert str)
)
(global-set-key "\M--" 'copy-preceding-line)

; ===========================================================================
;
; Sum the numbers at the beginning of each line in the region
;
(defun sum-column ()
  "Sum the column in the region"
  (interactive)
  (let (end
        hicol
        hipnt
        locol
        lopnt
        orig
        start
        sval
        tmp
        total)
    (setq total '0)
    (setq orig (point))
    (setq lopnt (point))
    (setq locol (column))

    (exchange-point-and-mark)
    (setq hipnt (point))
    (setq hicol (column))

    (if (< hicol locol)
        (progn
          (setq tmp hicol)
          (setq hicol locol)
          (setq locol tmp)
          )
      )

    (if (< hipnt lopnt)
        (progn
          (setq tmp hipnt)
          (setq hipnt lopnt)
          (setq lopnt tmp)
          )
      )

    (setq width (+ (- hicol locol) 1))
    (goto-char lopnt)

    (while (< (point) hipnt)
      (progn
        (setq ival (string-to-int
                    (buffer-substring (point) (+ (point) width))))
        (setq total (+ total ival))
        (forward-line 1)
        (beginning-of-line)
        (forward-char locol)
        )
      )
    (message "Total = %d" total)
    (goto-char orig)
    )
)

; ===========================================================================
;
(defun contact-make ()
  "Set up a contact file entry"
  (interactive)
  (setq contact-name
        (read-from-minibuffer "Contact Name? > "))
  (Info-edit)
  (search-forward "^^ add")
  (beginning-of-line)
  (forward-line -1)
  (insert "* " contact-name "::\n")

  (widen)
  (end-of-buffer)
  (insert "\n   Node: " contact-name ",   Up: Contact_Top, Next:\n\n   ")
  (search-backward "")
  (forward-line)
  (narrow-to-region (point) (point-max))
  (goto-char (point-max))
)

; ===========================================================================
;
(defun contact-entry ()
  "Display a form for constructing a new entry for the contacts file"
  (interactive)
  (switch-to-buffer "*entry*")
  (insert-file "~/info/contacts/entry")
  (end-of-line)
)

; ===========================================================================
;
(defun contact-store ()
  "Store a defined contact entry in the contacts file"
  (interactive)
  (beginning-of-buffer)

  (setq lines (count-lines (point-min) (point-max)))
  (message "lines: %d" lines)

  (while (< 0 lines)
    (setq here (point))
    (re-search-forward ":")
    (forward-char -1)
    (delete-char (- here (point)))
    (if (> (point) (point-min))
        (delete-char -1))
    (forward-line)
    (beginning-of-line)
    (setq lines (1- lines))
  )

  (goto-char (1+ (point-min)))
  (re-search-forward ":")
  (forward-char -1)
  (setq name (buffer-substring 1 (point)))

  (setq entry (buffer-string))
  (kill-buffer "*entry*")
  (find-file "~/info/contacts/contacts")
  (end-of-buffer)
  (insert entry)
  (save-buffer)

;  (setq command "ph " name)
;  (shell-command command)
)

; ===========================================================================
;
(defun xtr ()
  "Open the transaction record and position to the current week"
  (interactive)
  (find-file "~/misc/transactions")
  (re-search-forward "======")
  (tos)
  (re-search-forward "[0-9]$")
  (indent-for-tab-command)
  (indent-for-tab-command)
)

; ===========================================================================
;
(defun set-info-dir ()
  "Set the variable Info-directory"
  (interactive)
  (setq Info-directory (read-from-minibuffer
                        "New Info directory? > "
                        Info-directory))
  (setq Info-enable-edit 't)
)

; ===========================================================================
;
(defun column ()
  "Return point's current column"
  (let (c-here c-there)
    (setq c-here (point))
    (beginning-of-line)
    (setq c-there (point))
    (goto-char c-here)
    (- c-here c-there)
    )
)

(setq lisp-indent-offset 'nil)

; ===========================================================================
;
(defun line-length ()
  "Return length of current line"
  (interactive)
  (let (orig high low)
    (setq orig (point))
    (beginning-of-line)
    (setq low (point))
    (end-of-line)
    (setq high (point))
    (goto-char orig)
    (message "Line length: %d" (- high low))
    )
  )
(global-set-key "\C-xl" 'line-length)

; ===========================================================================
;
(defun x-line-length ()
  "Return length of current line"
  (let (orig high low)
    (setq orig (point))
    (beginning-of-line)
    (setq low (point))
    (end-of-line)
    (setq high (point))
    (goto-char orig)
    (- high low)
    )
  )

; ===========================================================================
;
(defun x-format ()
  "Format a field entry"
  (interactive)

  (if (< 21 (x-line-length))
      (message "Please abbreviate the entry name")
    (progn
      (beginning-of-line)
      (indent-for-tab-command)

                                        ; down case the field name
      (setq here (point))
      (end-of-line)
      (downcase-region here (point))

                                        ; join the next line
      (delete-char 1)
      (indent-to-column 24)

                                        ; measure and align the next item
      (setq here (point))
      (end-of-line)
      (setq length (- (point) here))
      (setq length (- 3 length))
      (setq istr (substring "        " 0 length))
      (goto-char here)
      (insert istr)

                                        ; join and align the next item
      (end-of-line)
      (delete-char 1)
      (indent-for-tab-command)

                                        ; join and align the next item
      (end-of-line)
      (delete-char 1)
      (indent-for-tab-command)

                                        ; join and align the next item
      (end-of-line)
      (delete-char 1)
      (indent-for-tab-command)
      (indent-for-tab-command)

                                        ; format the comment
      (end-of-line)
      (if (> (column) 79)
          (progn (beginning-of-line)
                 (forward-char 79)
                 (backward-word 1)
                 (insert "\n\n")
                 (indent-to-column 42)
                 (fill-paragraph 1)
                 (forward-line -1)
                 (kill-line 1)
                 (forward-paragraph 1))
        (forward-line 1)
        )

                                        ; position for the next one
      (kill-line 1)
      )
    )
  )

(global-set-key "\C-x," 'x-format)

; ===========================================================================
;
(setq my-template-dir "~/info/templates")
(defun new-perl ()
  "use a template to construct a new perl script"
  (interactive)
  (setq my-script (expand-file-name (read-file-name "script name? > ")))
  (find-file my-script)
  (insert-file (concat my-template-dir "/perl"))
  (replace-string "@Name@" my-script)
  (replace-string "@yyyy@" (format-time-string "%Y"))
  (replace-string "@yymmdd@" (format-time-string "%Y-%m-%d"))
  (goto-char (point-max))
  (search-backward "@what-string@")
  (save-buffer)
  (call-process "/bin/chmod" nil 't nil "a+x" my-script)
)

; ===========================================================================
;
(defun global-replace (my-from my-to)
  "replace all occurrences of MY-FROM with MY-TO"
  (interactive)
  (goto-char (point-min))
  (while (search-forward my-from nil t)
    (replace-match my-to nil t))
)

; ===========================================================================
;
(defun new-cxx ()
  "use a template to construct a new cxx source file"
  (interactive)
  (setq my-path (expand-file-name (read-file-name "file name? > ")))
  (setq my-classname (read-from-minibuffer "Class Name? > "))
  (setq my-file (basename my-path))
  (setq my-stem my-file)
  (if (string-match "\.cxx$" my-stem)
      (setq my-stem (replace-match "" nil nil my-stem))
      (progn (setq my-file (concat my-file ".cxx"))
             (setq my-path (concat my-path ".cxx"))
             ))

  (setq my-hpath my-path)
  (string-match "\.cxx$" my-hpath)
  (setq my-hpath (replace-match ".h" nil nil my-hpath))
  (setq my-hfile (basename my-hpath))
  (setq my-hdr my-hfile)
  (string-match "\\." my-hdr)
  (setq my-hdr (upcase (replace-match "_" nil nil my-hdr)))

  (find-file my-hpath)
  (insert-file (concat my-template-dir "/hdr.hxx"))
  (global-replace "@Name@" my-hfile)
  (global-replace "@yyyy@" (format-time-string "%Y"))
  (global-replace "@yymmdd@" (format-time-string "%Y-%m-%d"))
  (global-replace "@HEADER@" my-hdr)
  (global-replace "@ClassName@" my-classname)
  (save-buffer)

  (find-file my-path)
  (insert-file (concat my-template-dir "/mod.cxx"))
  (global-replace "@Name@" my-file)
  (global-replace "@yyyy@" (format-time-string "%Y"))
  (global-replace "@yymmdd@" (format-time-string "%Y-%m-%d"))
  (global-replace "@stem@" my-stem)
  (global-replace "@ClassName@" my-classname)
  (save-buffer)
  (goto-char (point-min))
)

; ===========================================================================
;
(defun new-cpp ()
  "use a template to construct a new cxx source file"
  (interactive)
  (setq my-path (expand-file-name (read-file-name "file name? > ")))
  (setq my-classname (read-from-minibuffer "Class Name? > "))
  (setq my-file (basename my-path))
  (setq my-stem my-file)
  (if (string-match "\.cpp$" my-stem)
      (setq my-stem (replace-match "" nil nil my-stem))
      (progn (setq my-file (concat my-file ".cpp"))
             (setq my-path (concat my-path ".cpp"))
             ))

  (setq my-hpath my-path)
  (string-match "\.cpp$" my-hpath)
  (setq my-hpath (replace-match ".h" nil nil my-hpath))
  (setq my-hfile (basename my-hpath))
  (setq my-hdr my-hfile)
  (string-match "\\." my-hdr)
  (setq my-hdr (upcase (replace-match "_" nil nil my-hdr)))

  (find-file my-hpath)
  (insert-file (concat my-template-dir "/hdr.hxx"))
  (global-replace "@Name@" my-hfile)
  (global-replace "@yyyy@" (format-time-string "%Y"))
  (global-replace "@yymmdd@" (format-time-string "%Y-%m-%d"))
  (global-replace "@HEADER@" my-hdr)
  (global-replace "@ClassName@" my-classname)
  (save-buffer)

  (find-file my-path)
  (insert-file (concat my-template-dir "/mod.cxx"))
  (global-replace "@Name@" my-file)
  (global-replace "@yyyy@" (format-time-string "%Y"))
  (global-replace "@yymmdd@" (format-time-string "%Y-%m-%d"))
  (global-replace "@stem@" my-stem)
  (global-replace "@ClassName@" my-classname)
  (save-buffer)
  (goto-char (point-min))
)

; ===========================================================================
;
(defun basename (my-path)
  "return the final component in the path"
  (interactive)
  (setq my-pt (string-match "[^/]*$" my-path))
  (substring my-path my-pt)
)

; ===========================================================================
;
(defun replicate-function ()
  "Replicate the function that currently contains point"
  (interactive)

  (setq page-delimiter "^{")

  ; (re-search-backward "(^{)|(^})" (point-min) 't
  (backward-page)
  (re-search-backward "^ *$" (point-min) 't)
  (forward-line)

  (setq start (point))
  (forward-page 2)
  (re-search-backward "^ *$" (point-min) 't)
  (forward-line)

  (insert (buffer-substring start (point)))
  (backward-page)
  (re-search-backward "^ *$" (point-min) 't)
  (forward-line)
)

; ---------------------------------------------------------------------------
(defun perl-sort-list ()
  "sort a list enclosed in parens"
  (interactive)

  (setq sl-origpoint (point))
  (setq sl-bs (buffer-substring (point) (+ (point) 1)))

  ; if search-backward fails, what is returned, and is point moved?
  ; -> nil is returned, point is not moved if arg 3 is 't

  ; if we're sitting on '(', start at next char, otherwise find '('+1
  (setq sl-start (if (string= sl-bs "(")
                     (+ (point) 1)
                   (if (numberp (setq sl-tmp (search-backward "(" 0 't)))
                       (+ sl-tmp 1)
                     -1)))

  ; hunt forward and send end to ')' - 1
  (if (not (numberp (setq sl-end (- (search-forward ")" (point-max) 't) 1))))
      (setq sl-end -1))

  (if (or (< sl-end sl-origpoint) (< sl-origpoint (- sl-start 1)))
      (setq sl-start -1))

  (setq sl-list "")
  (if (and (not (= sl-start -1)) (not (= sl-end -1)))
      (progn
        (setq sl-list (buffer-substring sl-start sl-end))
        (setq sl-list (split-string sl-list " *, *"))
        (setq sl-list (sort sl-list 'string-lessp))
        (setq sl-list (mapconcat (function (lambda (a) (format "%s" a)))
                                 sl-list ", "))
        (goto-char sl-start)
        (kill-region sl-start sl-end)
        (insert sl-list)
        )
    )

;  (message "s = %d; e = %d; l = '%s'" sl-start sl-end sl-list)
  (message "l = '%s'" sl-list)
  (goto-char sl-origpoint)
)

; ===========================================================================
(defun perl-localize ()
  "make the variable under the cursor local to the current function"
  (interactive)

; parse out the name of the current variable
  (setq original-point (point))
  (setq rx-char "[a-zA-Z0-9_]")
  (setq rx-vchar "[%$@]")
  (setq rx-var "[$@%][a-zA-Z_][a-zA-Z0-9_]*")
  (setq rx-mylist "^{\n+ +my +(")
  (setq rx-function "^{")

  (goto-char (+ (point) 1))
  (re-search-backward rx-vchar)
  (setq other-end (point))
  (re-search-forward rx-var)
  (setq varname (buffer-substring other-end (point)))

  (if (or (< original-point other-end) (< (- (point) 1) original-point))
      (progn
        (message "cursor is not in a variable name (op=%d,oe=%d,p=%d)"
                 original-point other-end (point))
        (goto-char original-point)
        )
      (progn
        (message "varname: '%s' (op=%d,oe=%d,p=%d)"
                 varname original-point other-end (point))
        (goto-char original-point)
        )
      )

; find the "my" at the beginning of the function, if there is one
  (re-search-backward rx-function)
  (if (not (string-match rx-mylist (buffer-substring (point) (+ 10 (point)))))
; if not, add it
      (progn
        ; (message "must add my() list")
        (goto-char (+ 1 (point)))
        (insert "\n   my (" varname ");\n")
        (goto-char (+ original-point (length varname) 11))
        )
; go to the end of "my (...^);
    (progn
      (setq start-mylist (point))
      (re-search-forward ");")
      (setq end-mylist (point))
      (setq varname_rgx (concat varname "[^a-zA-Z0-9_]"))
      (if (re-search-backward varname_rgx start-mylist 't)
          (progn
            (goto-char original-point)
            (message "%s is already local" varname)
            )
; insert the variable name
        (progn
          (re-search-backward "my +(")
          (re-search-forward "my +(")
          (insert varname ", ")
          (perl-sort-list)
; return to (+ point (length name))
          (goto-char (+ original-point (length varname) 2))
          (message "%s added to my list" varname)
          )
        )
      )
    )
)
(global-set-key "\C-x\C-l" 'perl-localize)


; ---------------------------------------------------------------------------
(defun indent-to-here (column)
  "With argument, indent to column. Without, remove whitespace to next non-whitespace, i.e., indent to current column"
  (interactive "P")
  (let (ccol)
    (setq ccol (current-column))
    (if (equal column nil)
        (progn
          (just-one-space)
          (indent-to-column ccol)
        )
      (if (< ccol column)
          (indent-to-column column)
        (progn
          (move-to-column column)
          (just-one-space)
          (indent-to-column column)
        )
      )
    )
  )
)
(global-set-key "\C-x\C-i" 'indent-to-here)
; (debug-on-entry 'indent-to-here)

; ---------------------------------------------------------------------------
(defun dquote ()
  "If text is selected, add double quotes around it. Otherwise, if
   we're in a quoted string, remove innermost quotes. Otherwise, do
   nothing."
  (interactive)
  ; (debug)
  (save-excursion
    (if (use-region-p)
        ; quote the region
        (progn (insert "\"")
             (exchange-point-and-mark)
             (insert "\"")
             (exchange-point-and-mark)
        )
        ; remove quotes if in a quoted string
        (unquote)
    )
  )
)
(global-set-key "\M-\"" 'dquote)

; ---------------------------------------------------------------------------
(defun bracket ()
  "If text is selected, add brackets around it. Otherwise, fail
   with a message."
  (interactive)
  ; (debug)
  (save-excursion
    (if (use-region-p)
        ; quote the region
        (if (< (point) (mark))
            (progn (insert "[")
                   (exchange-point-and-mark)
                   (insert "]")
                   (exchange-point-and-mark))
            (progn (exchange-point-and-mark)
                   (insert "[")
                   (exchange-point-and-mark)
                   (insert "]"))
            )
      (message "select the text to be enclosed")
    )
  )
)
(global-set-key "\M-[" 'bracket)

; ---------------------------------------------------------------------------
(defun squote ()
  "If text is selected, add single quotes around it. Otherwise, if
   we're in a quoted string, remove innermost quotes. Otherwise, do
   nothing."
  (interactive)
  ; (debug)
  (save-excursion
    (if (use-region-p)
        ; quote the region
        (progn (insert "'")
             (exchange-point-and-mark)
             (insert "'")
             (exchange-point-and-mark)
        )
        ; remove quotes if in a quoted string
        (unquote)
    )
  )
)
(global-set-key "\M-'" 'squote)

; ---------------------------------------------------------------------------
(defun unquote ()
  "Remove a set of single or double quotes surrounding the current string"
  (interactive)
  (let (start
        quote
        fexp)
    (progn
      (re-search-backward "['\"\n]")
      (if (equal (string (char-after)) "\'")
          (progn (setq start (point))
                 (setq quote "'")
                 (setq fexp "['\n]")
          )
          (if (equal (string (char-after)) "\"")
              (progn (setq start (point))
                     (setq quote "\"")
                     (setq fexp "[\"\n]")
              )
              (return)
          )
      )
      (re-search-forward fexp)
      (re-search-forward fexp)
      (if (equal (string (char-before)) quote)
          (progn (delete-char -1)
                 (goto-char start)
                 (delete-char 1)
          )
      )
    )
  )
)
(global-set-key "\C-X'" 'unquote)

; ---------------------------------------------------------------------------
(defun fcomment ()
  "Bracket the current function, then either comment it or uncomment it"
  (interactive)
  ; search back to "^(# )?def", set mark
  (message "%d" (point))
  (re-search-backward "^def ")
  ; (set-mark-command (point))
  ; (forward-line)
  ; (re-search-forward "^\\(# *\\)?def ")
  ; search forward to "^(# )?def"
  ; search backward to an empty line
  ; search back to a non-empty line
  ; call comment-region (with an arg if already commented)
  ;; ; (debug)
  ;; (save-excursion
  ;;   (if (use-region-p)
  ;;       ; quote the region
  ;;       (progn (insert "\"")
  ;;            (exchange-point-and-mark)
  ;;            (insert "\"")
  ;;            (exchange-point-and-mark)
  ;;       )
  ;;       ; remove quotes if in a quoted string
  ;;       (unquote)
  ;;   )
  ;; )
)
(global-set-key "\M-#" 'fcomment)

; ---------------------------------------------------------------------------
(defun reload-dot-emacs ()
  "Reload the file $HOME/.emacs"
  (interactive)
  (setq home (getenv "HOME"))
  (setq path (concat home "/.emacs"))
  (load-file path)
  (message "Reloaded %s" path)
  )
(global-set-key "\C-c\C-r" 'reload-dot-emacs)

;; ----------------------------------------------------------------------------
(defun qrepl (before new mod)
  "Replace BEFORE with NEW iteratively modified per MOD"
  (interactive "nSearch:\nnFirst:\nxMod:")
  (while (re-search-forward (number-to-string before) nil 't)
    (replace-match (number-to-string new))
    (setq new (eval-expression mod))
  ))

;; ----------------------------------------------------------------------------
(defun tos ()
  "Move the current line to the top of the screen"
  (interactive)
  (recenter 0)
)
