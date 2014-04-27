;;; ---------------------------------------------------------------
;;; Generate a weekly status report
;;; ---------------------------------------------------------------
(defun status ()
  "Generate a weekly status report"
  (interactive)  
  (setq original-buffer (buffer-name))
  (setq st-diary-dir "/acct/tom/history/diary/")
  (set-buffer (get-buffer-create "*work*"))

  (erase-buffer)
  (call-process "dt" nil 't nil "%a")
  (backward-char 1)
  (setq st-weekday (buffer-substring 1 (point-marker)))

  (erase-buffer)
  (if (string-equal st-weekday "Fri")
      (progn 
        (call-process "dt" nil 't nil "%y%m%d")
        )
      (progn
        (call-process "dt" nil 't nil "%y%m%d" "last" "friday")
        )
    )
;  (call-process "dt" nil 't nil st-dt-parm)
  (backward-char 1)
  (setq st-filename (buffer-substring 1 (point-marker)))
  (setq st-filename (concat "/acct/tom/history/status/status_" st-filename))

  (setq orig-ldbs list-directory-brief-switches)
  (setq list-directory-brief-switches "")
  (list-directory "~/history/status")
  (set-buffer "*Directory*")
  (end-of-buffer)
  (forward-char -1)
  (setq end (point))
  (beginning-of-line)
  (setq st-LastStatName (buffer-substring (point) end))
  (setq list-directory-brief-switches orig-ldbs)

  (erase-buffer)
  (call-process "/local/bin/dt" nil 't nil "%y/%m/%d" "last" "monday")
  (backward-char 1)
  (setq st-monday (buffer-substring 1 (point-marker)))
  (setq st-monday (concat st-diary-dir st-monday))

  (erase-buffer)
  (call-process "/local/bin/dt" nil 't nil "%y/%m/%d" "last" "tuesday")
  (backward-char 1)
  (setq st-tuesday (buffer-substring 1 (point-marker)))
  (setq st-tuesday (concat st-diary-dir st-tuesday))

  (erase-buffer)
  (call-process "/local/bin/dt" nil 't nil "%y/%m/%d" "last" "wednesday")
  (backward-char 1)
  (setq st-wednesday (buffer-substring 1 (point-marker)))
  (setq st-wednesday (concat st-diary-dir st-wednesday))

  (erase-buffer)
  (call-process "/local/bin/dt" nil 't nil "%y/%m/%d" "last" "thursday")
  (backward-char 1)
  (setq st-thursday (buffer-substring 1 (point-marker)))
  (setq st-thursday (concat st-diary-dir st-thursday))

  (erase-buffer)
  (call-process "/local/bin/dt" nil 't nil "%y/%m/%d")
  (backward-char 1)
  (setq st-friday (buffer-substring 1 (point-marker)))
  (setq st-friday (concat st-diary-dir st-friday))

  (erase-buffer)
  (call-process "/local/bin/dt" nil 't nil "%y/%m/%d" "last" "monday")
  (backward-char 1)
  (setq st-startweek (buffer-substring 1 (point-marker)))

  (erase-buffer)
  (call-process "dt" nil 't nil "%a")
  (backward-char 1)
  (setq st-weekday (buffer-substring 1 (point-marker)))

  (erase-buffer)
  (if (string-equal st-weekday "Fri")
      (progn 
        (call-process "dt" nil 't nil "%y/%m/%d")
        )
      (progn
        (call-process "dt" nil 't nil "%y/%m/%d" "last" "friday")
        )
    )
;  (call-process "/local/bin/dt" nil 't nil "%m/%d/%y")
  (backward-char 1)
  (setq st-endweek (buffer-substring 1 (point-marker)))

  (find-file st-filename)
  (text-mode)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (goto-char (point-max))

  (condition-case nil
      (insert-file st-LastStatName)
    (error nil))
  
  (goto-char (point-min))
  (search-forward "(")
  (setq there (point))
  (search-forward ")")
  (backward-char 1)
  (setq here (point))
  (delete-region there here)
  (insert st-startweek " - " st-endweek)

  (goto-char (point-max))

  (condition-case nil
;      (insert-file st-monday)
      (call-process "do_comb" st-monday t nil)
    (error nil))
  (goto-char (point-max))

  (condition-case nil
;      (insert-file st-tuesday)
      (call-process "do_comb" st-tuesday t nil)
    (error nil))
  (goto-char (point-max))

  (condition-case nil
;      (insert-file st-wednesday)
      (call-process "do_comb" st-wednesday t nil)
    (error nil))
  (goto-char (point-max))

  (condition-case nil
;      (insert-file st-thursday)
      (call-process "do_comb" st-thursday t nil)
    (error nil))
  (goto-char (point-max))

  (condition-case nil
;      (insert-file st-friday)
      (call-process "do_comb" st-friday t nil)
    (error nil))

  (search-backward "======")
  (tos)

  (message "\"mail-status\" will mail the status report")
)

;;; ---------------------------------------------------------------
;;; Send a weekly status report
;;; ---------------------------------------------------------------
(defun mail-status ()
  "Mail the current buffer as a status report"
  (interactive)  

  (setq status-msg (buffer-string))

  (mail)
  (insert "art@condor")
  (search-forward "Subject: ")
  (insert "Weekly Status Report")

  (forward-line 2)
  (insert status-msg)
  (mail-send)
)

