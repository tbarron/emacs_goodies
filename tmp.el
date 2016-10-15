 ; ---------------------------------------------------------------------------
(defun goto-fileloc-at-point ()
  "Jump to the file location indicated at point"
  (interactive)
  (if (not (string-equal (buffer-name) "*compilation*"))
      (switch-to-buffer-other-window "*compilation*"))
  (if (equal (point) (point-min))
      (goto-char (point-max)))
  (save-match-data
    ; (beginning-of-line)
    (re-search-backward "^[^ \r\t\n:][^ \r\t\n:]*:[0-9][0-9]*:[0-9][0-9]*: .*")
    ; (forward-line -1)
    (let ((ms (match-string 0)))
      (let ((filename (nth 0 (split-string ms ":")))
            (line (nth 1  (split-string ms ":")))
            (col (nth 2 (split-string ms ":")))
            (why (nth 3 (split-string ms ":")))
            )
        (find-file-other-window filename)
        (goto-line (string-to-number line))
        (move-to-column (- (string-to-number col) 1))
        (message why)
        )
      )
    )
  )
(global-set-key "\M-f" 'goto-fileloc-at-point)

; ---------------------------------------------------------------------------
(defun align-next-line ()
  "Attempt to align the nearest word on the next line"
  (interactive)
  (setq anl-col (column))
  (forward-line 1)
  (if (< anl-col (line-length))
      (progn
        (forward-char anl-col)
        (setq anl-chr (buffer-substring (point) (- (point) 1)))
        (if (not (string-equal anl-chr " "))
            (progn
              (forward-word -1)
              (setq anl-diff (- anl-col (column)))
              (insert (substring "                      " 0 anl-diff))
            )
            (progn
              (setq anl-chr (buffer-substring (point) (+ (point) 1)))
              (if (string-equal anl-chr " ")
                  (progn (forward-word 1) (forward-word -1))
              )
              (setq anl-diff (- (column) anl-col))
              (delete-char (- 0 anl-diff))
            )
        )
      )
      (end-of-line)
  )
  ; (message "anl-chr: '%s'" anl-chr)
  ; (if (
)
(global-set-key "\C-x\\" 'align-next-line)

; ---------------------------------------------------------------------------
(defun global-word-downcase ()
  "Downcase the word under the cursor everywhere it appears in the current buffer"
  (interactive)
  (setq srch-exp "[^A-Z_.]")
  (re-search-forward srch-exp)
  (re-search-backward srch-exp)
  (setq there (point))
  (re-search-backward srch-exp)
  (re-search-forward srch-exp)
  (setq uword (buffer-substring (point) there))
  (setq lword (downcase uword))
  (query-replace uword lword)
)

; ---------------------------------------------------------------------------
(defun line-length ()
  "Compute and return the length of the current line"
  (interactive)
  (setq ll-where (point))
  (beginning-of-line)
  (setq ll-low (point))
  (end-of-line)
  (setq ll-high (point))
  (goto-char ll-where)
  (setq ll-length (- ll-high ll-low))
;  (message "length: %d" length)
)

; ---------------------------------------------------------------------------
(defun nxlongline ()
  "Go to the next line longer than 79 chars, break it at a space, and indent the new line"
  (interactive)
  (while (< (line-length) 80)
    (next-line 1)
    )
  (beginning-of-line)
  (forward-char 80)
  (search-backward " ")
  (forward-char 1)
  (newline)
  (tab-to-tab-stop)
)

; ---------------------------------------------------------------------------
(defun findll ()
  "find the next line longer than 79 chars"
  (interactive)
  (while (< (line-length) 80)
    (next-line 1)
    )
  (end-of-line)
)

; ---------------------------------------------------------------------------
(defun number-entries ()
  "Scan through a file, inserting a number into each entry"
  (interactive)
  (setq number 5)
  (while
      (search-forward " ==")
    (setq s-num (format " %04d " number))
    (insert s-num)
    (delete-char (length s-num))
    (setq number (+ number 1))
    )
)

; ---------------------------------------------------------------------------
(defun insert-filename ()
  "Inserts the current absolute file path at point"
  (interactive)
  (insert (buffer-file-name))
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

; ---------------------------------------------------------------------------
(defun py-comment-function ()
  ""
  (interactive)

  (re-search-forward "^ *def ")
  (end-of-line)
  (insert "\n\"\"\"")
  (py-indent-line)
  (insert "\n\"\"\"")
  (py-indent-line)
  (forward-line -1)
  (end-of-line)
  (insert "\n")
  (py-indent-line)
)
(global-set-key "\M-f" 'py-comment-function)
