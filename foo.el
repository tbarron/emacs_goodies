# ---------------------------------------------------------------------------
(defun sort-list ()
  "sort a list enclosed in parens"
  (interactive)

;  (setq sl-origpoint (point))
;  (setq sl-bs (buffer-substring (point) (+ (point) 1)))

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

  (setq sl-list "")
  (if (and (not (= sl-start -1)) (not (= sl-end -1)))
      (progn
        (setq sl-list (buffer-substring sl-start sl-end))
        )
    )
        
;  (message "s = %d; e = %d; l = '%s'" sl-start sl-end sl-list)
;  (goto-char sl-origpoint)
)

