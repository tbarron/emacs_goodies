;; Support mode, and its ideosyncratic commands.

(defvar support-mode-syntax-table nil
  "Syntax table used while in support mode.")

(defvar support-mode-abbrev-table nil
  "Abbrev table used while in support mode.")
(define-abbrev-table 'support-mode-abbrev-table ())

(if support-mode-syntax-table
    ()
  (setq support-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " support-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " support-mode-syntax-table)
  (modify-syntax-entry ?' "w   " support-mode-syntax-table))

(defvar support-mode-map nil "")
(if support-mode-map
    ()
  (setq support-mode-map (make-sparse-keymap))
  (define-key support-mode-map "\t" 'tab-to-tab-stop)
  (define-key support-mode-map "\es" 'support-sub-entry)
  (define-key support-mode-map "\ee" 'support-new-entry)
  (define-key support-mode-map "\em" 'support-cut-entry)
  (define-key support-mode-map "\ea" 'support-copy-entry)
  (define-key support-mode-map "\ep" 'support-yank-entry)
)


;(defun non-saved-text-mode ()
;  "Like text-mode, but delete auto save file when file is saved for real."
;  (text-mode)
;  (make-local-variable 'delete-auto-save-files)
;  (setq delete-auto-save-files t))

;;; ---------------------------------------------------------------
;;; support-mode
;;; ---------------------------------------------------------------
(defun support-mode ()
  "Major mode for editing my support files.  Special commands:\\{support-mode-map}
Turning on support-mode calls the value of the variable support-mode-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map support-mode-map)
  (setq mode-name "Support")
  (setq major-mode 'support-mode)
  (setq local-abbrev-table support-mode-abbrev-table)
  (set-syntax-table support-mode-syntax-table)
  (run-hooks 'support-mode-hook))

;; ===========================================================================
;
; Open a support file.  "dg" is the default vendor
;
(defun support ()
  "Open a support file"
  (interactive)
  
  (find-file (concat 
              "~/history/"
              (read-from-minibuffer "Vendor? > " "dg") 
              ".support"))
  (goto-char (point-max))
)

;;; ---------------------------------------------------------------
;;; support-search-exp
;;; ---------------------------------------------------------------
(defun support-search-exp ()
  (setq search-exp "^==========*$")
)

;;; ---------------------------------------------------------------
;;; support-cut-entry
;;; ---------------------------------------------------------------
(defun support-cut-entry ()
  "Cut the current entry to the kill ring for subsequent yanking"
  (interactive)
  (support-mark-current-entry)
  (setq entry (buffer-substring start (point)))
  (kill-region start (point))
)

;;; ---------------------------------------------------------------
;;; support-copy-entry
;;; ---------------------------------------------------------------
(defun support-copy-entry ()
  "Copy the current entry to the kill ring for subsequent yanking"
  (interactive)
  (support-mark-current-entry)
  (copy-region-as-kill start (point))
)

;;; ---------------------------------------------------------------
;;; support-yank-entry
;;; ---------------------------------------------------------------
(defun support-yank-entry ()
  "Yank the most recent kill from the kill ring before the current entry"
  (interactive)
  (support-beginning-of-entry)
  (yank)
)

;;;; ---------------------------------------------------------------
;;; support-new-entry
;;; ---------------------------------------------------------------
(defun support-new-entry ()
  "Create a new support entry with today's date"
  (interactive)
  (goto-char (point-max))
  (insert "\n" 
          "======================================="
          "======================================="
          "\n")
  (dt-time)
  (insert "    ref = "
          "\n  description: \n")
  (search-backward "ref = ")
  (end-of-line)
)

;;; ---------------------------------------------------------------
;;; sub-entry
;;; ---------------------------------------------------------------
(defun support-sub-entry ()
  "Create a sub-entry within the current support entry"
  (interactive)
;  (setq search-exp "^[-x><+]")
  (support-search-exp)
  (forward-char 1)
  (if (re-search-forward search-exp nil 'x)
      (backward-char 1)
    )
  (re-search-backward "[]a-zA-Z.;,:0-9?()]")
  (forward-char 1)
  (newline)
  (insert "\n------> ")
  (dt-time)
  (insert "\n\n   ")
  (setq fill-prefix "   ")
)

;;; ---------------------------------------------------------------
;;; goto today's dodo file in the current buffer
;;; ---------------------------------------------------------------
;(defun goto-today ()
;  "Jump to today's dodo file"
;  (interactive)
;  (find-file "~/.dodo/today_have_to.do")
;)

;;; ---------------------------------------------------------------
;;; bracket the current entry - set mark start at beginning and put
;;; point at the end
;;; ---------------------------------------------------------------
(defun support-mark-current-entry ()
  (if (not (equal (point) (point-max)))
      (forward-char 1))                  ; mark beginning of entry
;  (setq search-exp "^[-x><+]")
  (support-search-exp)
  (re-search-backward search-exp)
  (setq start (point-marker))
  (forward-char 1)
  (if (re-search-forward search-exp nil 'x)   ; go to end of entry
      (backward-char 1)
  )
)

;;; ---------------------------------------------------------------
;;; go to the beginning of the current support entry
;;; ---------------------------------------------------------------
(defun support-beginning-of-entry ()
  (if (not (equal (point) (point-max)))
      (forward-char 1))                  ; mark beginning of entry
;  (setq search-exp "^[-x><+]")
  (support-search-exp)
  (re-search-backward search-exp)
)
