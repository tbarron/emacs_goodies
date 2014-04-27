;; A collection of macros for interfacing to sccs

;(global-set-key "\C-x\C-g" 'sccs-get)
;(global-set-key "\C-x\C-u" 'sccs-unget)
;(global-set-key "\C-x\C-v" 'sccs-view)
;(global-set-key "\C-x\C-d" 'sccs-delta)
;(global-set-key "\C-x\C-a" 'sccs-admin)
;(global-set-key "\C-x\C-p" 'sccs-prs)

;;; ---------------------------------------------------------------
;;; sccs-get: get a file for editing
;;; ---------------------------------------------------------------
(defun sccs-get ()
  "Get an sccs file for editing."
  (interactive)
  (sccs-file-name "get -e file: ")             ; set sfn and fn
  (call-process "get" nil nil nil "-e" sfn)
  (if (file-exists-p fn)
      (find-file fn)
      (message "Failure extracting file %s" fn)
      )
)

;;; ---------------------------------------------------------------
;;; sccs-unget: unget a file for editing
;;; ---------------------------------------------------------------
(defun sccs-unget ()
  "Unget an sccs file."
  (interactive)

;  (sccs-file-name "unget file: ")

  (save-buffer)
  (setq init (concat (expand-file-name ".") "/" (buffer-name)))
  (setq fn (read-file-name "unget file: " init))
  (setq dir (file-name-directory fn))
  (setq fn (file-name-nondirectory fn))
  (if (not (string-equal (substring fn 0 2) "s."))
      (setq sfn (format "s.%s" fn))
      (progn (setq sfn fn) (setq fn (substring fn 2)))
    )

;  (read-minibuffer (format "file ref: %s" (concat dir sfn)))
  (call-process "unget" nil nil nil sfn)
  (if (or (file-exists-p fn) (file-exists-p (concat "p." fn)))
      (message "Failure ungetting file %s" fn)
      (progn
        (kill-buffer fn)
        (kill-buffer "##sccs##")
      )
  )
)

;;; ---------------------------------------------------------------
;;; sccs-view: get a reference copy of a file
;;; ---------------------------------------------------------------
(defun sccs-view ()
  "Get a reference copy of an sccs file for viewing."
  (interactive)
  (sccs-file-name "get file: ")             ; set sfn and fn
  (call-process "get" nil nil nil sfn)
  (if (file-exists-p fn)
      (progn (view-file fn)
             (call-process "rm" nil nil nil "-f" fn)
             (kill-buffer "##sccs##")
             )
      (message "Failure extracting file %s" fn)
      )
)

;;; ---------------------------------------------------------------
;;; sccs-delta
;;; ---------------------------------------------------------------
(defun sccs-delta ()
  "Delta the current buffer into an sccs file."
  (interactive)

  (save-buffer)
  (setq init (concat (expand-file-name ".") "/" (buffer-name)))
  (setq fn (read-file-name "delta file: " init))
  (setq dir (file-name-directory fn))
  (setq fn (file-name-nondirectory fn))
  (if (not (string-equal (substring fn 0 2) "s."))
      (setq sfn (format "s.%s" fn))
      (progn (setq sfn fn) (setq fn (substring fn 2)))
    )

  (setq commentary (read-string "comments? "))
  (set-buffer (get-buffer-create "##sccs##"))
  (erase-buffer)
  (insert commentary)
  (newline)
  (save-buffer)
  (call-process "delta" "##sccs##" nil nil sfn)
  (if (or (file-exists-p fn) (file-exists-p (concat "p." fn)))
      (message "Failure deltaing file %s" fn)
      (progn
        (kill-buffer fn)
        (kill-buffer "##sccs##")
        (call-process "rm" nil nil nil "##sccs##*")
      )
  )
)

;;; ---------------------------------------------------------------
;;; sccs-admin
;;; ---------------------------------------------------------------
(defun sccs-admin ()
  "Run the sccs admin command to create and administer sccs files."
  (interactive)
  (sccs-file-name "admin file: ")
  (setq options (format "%s" (read-minibuffer "options? ")))
  (call-process "admin" nil nil nil options sfn)
  (if (not (file-exists-p sfn))
      (message "Failure admining file %s" fn)
      (progn
        (kill-buffer "##sccs##")
      )
  )
)

;;; ---------------------------------------------------------------
;;; sccs-prs
;;; ---------------------------------------------------------------
(defun sccs-prs ()
  "Display an sccs prs report on an sccs file."
  (interactive)
  (sccs-file-name "prs file: ")
  (setq fn (concat fn ".prs"))
  (set-buffer (get-buffer-create fn))
  (call-process "prs" nil 't 't sfn)
  (beginning-of-buffer)
;  (debug)
  (view-buffer fn)
  (if (bufferp (get-buffer fn)) (kill-buffer fn))
  (if (bufferp (get-buffer "##sccs##")) (kill-buffer "##sccs##"))
)

;;; ---------------------------------------------------------------
;;; sccs-file-name
;;; ---------------------------------------------------------------
(defun sccs-file-name (prompt)
  (setq init (concat (expand-file-name ".") "/"))
  (setq fn (read-file-name prompt init))
  (setq dir (file-name-directory fn))
  (setq fn (file-name-nondirectory fn))
  (if (not (string-equal (substring fn 0 2) "s."))
      (setq sfn (format "s.%s" fn))
      (progn (setq sfn fn) (setq fn (substring fn 2)))
    )
  (find-file (concat dir "##sccs##"))
)

