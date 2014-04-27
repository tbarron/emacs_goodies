(setq html-mode-hook
          '(lambda ()
             (define-key html-mode-map "\M-i" 'html-p-li)
           )
)

(defun new-html ()
  "create a new html file"
  (interactive)
  (setq h-filename (expand-file-name (read-file-name "New filename? > ")))
  (find-file h-filename)
  (insert "<html>\n")
  (insert "  <head>\n")
  (insert "    <title></title>\n")
  (insert "  </head>\n")
  (insert "  <body>\n")
  (insert "  </body>\n")
  (insert "</html>\n")
  (goto-char 28)
)

(defun html-p-li ()
  "insert a <p><li> entry"
  (interactive)
  (insert "\n  <p><li>")
)

(defun html-mailto-link ()
  "wrap the marked region in a mailto: link"
  (interactive)
  (setq mml-addr (buffer-substring (point) (mark)))
  (setq mml-here (point))

  (if (< (mark) (point))
      (goto-char (mark)))

  (insert "<a href=\"")
  (insert mml-addr)
  (insert "\">")

  (if (< (mark) mml-here)
      (goto-char mml-here)
      (goto-char (mark)))

  (insert "</a>")

  (message "addr is '%s'" addr)
)
