;; A collection of macros for interfacing to the unix command line

(global-set-key "" 'man)

;;; ---------------------------------------------------------------
;;; Get a man page and view it
;;; ---------------------------------------------------------------
(defun man ()
  "Get a man page and view it."
  (interactive)
  (setq topic (read-minibuffer "man topic: "))
  (setq topic (format "%s" topic))
  (set-buffer (get-buffer-create "*man*"))
  (erase-buffer)
  (call-process "/usr/bin/man" nil "*man*" nil "-Tunknown" topic)
  (beginning-of-buffer)
  (view-buffer "*man*")
  (if (bufferp (get-buffer "*man*")) (kill-buffer "*man*"))
)

;;; ---------------------------------------------------------------
;;; Get the crontab for editing
;;; ---------------------------------------------------------------
(defun get-crontab ()
  "Edit the crontab."
  (interactive)
;  (set-buffer (get-buffer-create "*crontab*"))
  (switch-to-buffer (get-buffer-create "*crontab*"))
  (erase-buffer)
  (call-process "/usr/bin/crontab" nil "*crontab*" nil "-l")
)

;;; ---------------------------------------------------------------
;;; Save the crontab after editing
;;; ---------------------------------------------------------------
(defun save-crontab ()
  "Save the crontab."
  (interactive)
  (switch-to-buffer (get-buffer-create "*crontab*"))
  (write-file "/tmp/crontab")
  (call-process "/usr/bin/crontab" nil nil nil "/tmp/crontab")
  (kill-buffer "crontab")
  (call-process "/bin/rm" nil nil nil "/tmp/crontab")
)
