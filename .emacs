;;;; ===============================================================
;;;; Variable settings
;;;; ===============================================================
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-start "[Uu]pdated:[ 	]+\\\\?[\"<]+")
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S")
; (setq time-stamp-format "%04y-%02m-%02d")
(setq text-mode-hook
   '(lambda () 
      (auto-fill-mode 1) 
      (setq tab-width 4)))
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space 'nil)

(setq-default comment-empty-lines 't
              c-indent-tabs-mode t
              c-indent-level 4
              c-basic-offset 4)

; C mode
(setq c-mode-hook
      '(lambda ()
         (setq page-delimiter "^{")
         (setq-default c-electric-flag nil
                       comment-empty-lines 't
                       c-indent-tabs-mode t
                       c-indent-level 4
                       )
         (setq c-default-style "linux"
               c-basic-offset 4)
         (c-set-offset 'arglist-intro '+)
         (c-set-offset 'arglist-cont 0)
         (c-set-offset 'arglist-close 0)
         (c-set-offset 'case-label '+)
         (c-set-offset 'statement-cont 0)
         (c-set-offset 'substatement-open 0)
         (c-set-offset 'substatement '+)
         (local-set-key "\M-#" 'compile)
         )
)

(setq java-mode-hook
   '(lambda ()
      (setq tab-width 4)
      ; (c-set-style "Ellemtel")
      ))
;; (setq c++-mode-hook
;;    '(lambda ()
;;       (setq page-delimiter "^{")
;;       (setq tab-width 4)
;;       (local-set-key "\M-#" 'compile)
;;       (local-set-key "\t" 'c-indent-command)
;;       (c-set-style "Ellemtel")
;;       ))
(setq perl-mode-hook
   '(lambda ()
      (setq perl-indent-level 3)
      (setq perl-brace-offset -3)
      (setq perl-brace-imaginary-offset -3)
      (setq perl-continued-brace-offset 0)
      (setq perl-continued-statement-offset 3)
      ))
(setq tcl-mode-hook
   '(lambda ()
      (setq tcl-indent-level 4)
      ))
(setq do-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      (setq tab-width 3)
      (setq fill-prefix "   ")))
(setq view-hook
   '(lambda ()
      (local-set-key "\C-t" 'View-scroll-lines-backward)
      (local-set-key "\C-w" 'other-window)))
(setq support-mode-hook
   '(lambda () 
      (auto-fill-mode 1) 
      (setq tab-width 3)))
(setq visible-bell 't)
(setq-default indent-tabs-mode 'nil)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                       64 68 72 76))
(setq dired-listing-switches "-alct")
(setq c-brace-offset -2)
(setq c-argdecl-indent 4)
(setq c-indent-level 4)
(setq lisp-indent-offset '-2)
(setq auto-mode-alist (cons (quote (".p$" . c-mode)) auto-mode-alist))
; (setq auto-mode-alist (cons (quote ("README$" . do-mode)) auto-mode-alist))
; (setq auto-mode-alist (cons (quote (".done$" . org-mode)) auto-mode-alist))
; (setq auto-mode-alist (cons (quote ("[dD][oO]$" . org-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote (".ol$" . outline-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote (".doc$" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote (".msg$" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote (".b$" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote (".e$" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote (".ul$" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote ("cvs......" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote ("log.[0-9]+" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote ("sscr.[0-9]+" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (cons (quote (".php$" . tcl-mode)) auto-mode-alist))

(setq dired-listing-switches "-l")
(setq mail-yank-prefix "> ")
;;;; ===============================================================
;;;; Files to load
;;;; ===============================================================
(if (file-exists-p "~/Dropbox/el/diary.el") 
    (load-library "~/Dropbox/el/diary.el"))
(if (file-exists-p "~/Dropbox/el/do-mode.el") 
    (load-library "~/Dropbox/el/do-mode.el"))
(if (file-exists-p "~/Dropbox/el/html.el") 
    (load-library "~/Dropbox/el/html.el"))
(if (file-exists-p "~/Dropbox/el/python-mode.el") 
    (load-library "~/Dropbox/el/python-mode.el"))
(if (file-exists-p "~/Dropbox/el/sccs.el") 
    (load-library "~/Dropbox/el/sccs.el"))
(if (file-exists-p "~/Dropbox/el/status.el") 
    (load-library "~/Dropbox/el/status.el"))
(if (file-exists-p "~/Dropbox/el/support.el") 
    (load-library "~/Dropbox/el/support.el"))
(if (file-exists-p "~/Dropbox/el/tags.el") 
    (load-library "~/Dropbox/el/tags.el"))
(if (file-exists-p "~/Dropbox/el/tools.el") 
    (load-library "~/Dropbox/el/tools.el"))
(if (file-exists-p "~/Dropbox/el/unix.el") 
    (load-library "~/Dropbox/el/unix.el"))
(if (file-exists-p "~/Dropbox/el/mykeys.el") 
    (load-library "~/Dropbox/el/mykeys.el"))

(setq python-mode-hook
    '(lambda ()
       (setq-default comment-empty-lines 't)
       (setq fill-column 79)
       )
)

;;;; ===============================================================
;;;; Key bindings
;;;; ===============================================================
;(global-set-key "DEL" 'backward-delete-char-untabify)
(global-set-key "\C-a" 'backward-word)
(global-set-key "\C-d" 'kill-word)
(global-set-key "\C-g" 'forward-word)
(global-set-key "\C-h" 'backward-delete-char-untabify)
(global-set-key "\C-l" 'tos)
(global-set-key "\C-s" 'repeat-last-search)
(global-set-key "\C-r" 'repeat-search-backward)
(global-set-key "\C-t" 'scroll-down)
(global-set-key "\C-^" 'scroll-down)
(global-set-key "\C-w" 'other-window)
;(global-set-key "\C-?" 'delete-char)

(global-set-key "\C-c\C-c" 'keyboard-quit)
(global-set-key "\C-c\C-l" 'add-change-log-entry)

(global-set-key "\C-x " 'just-one-space)
(global-set-key "\C-x\C-h" 'delete-horizontal-space)
(global-set-key "\C-x[" 'backward-paragraph)
(global-set-key "\C-x]" 'forward-paragraph)
(global-set-key "\C-x>" 'do-search-forward)
(global-set-key "\C-x<" 'do-search-backward)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-x\C-d" 'do-visit-done)
(global-set-key "\C-x\C-e" 'dt-mdy)
;(global-set-key "\C-x\C-i" 'indent-rigidly)
;(global-set-key "\C-x!" 'new-issue)
(global-set-key "\C-x\C-k" 'kill-all-user-buffers)
(global-set-key "\C-x\C-n" 'repeat-complex-command)
(global-set-key "\C-x\C-r" 'insert-file)
(global-set-key "\C-x\C-t" 'goto-today)
(global-set-key "\C-x\C-y" 'copy-region-as-kill)
(global-set-key "\C-x," 'tags-search)
(global-set-key "\M-/" 'find-tag-other-window)
;(global-set-key "\C-xt" 'dt-date)
;(global-set-key "\M-n" 'dt-time)
(global-set-key "\C-x\C-d" 'dt-date)
(global-set-key "\C-x\C-t" 'dt-time)
(global-set-key "\C-xD" 'dt-datetime)
(global-set-key "\C-xy" 'dt-yesterday)

(global-set-key "\C-xe" 'diary-visit-defects)
(global-set-key "\C-xi" 'diary-issues)
(global-set-key "\C-xj" 'diary)
(global-set-key "\C-xl" 'redraw-display)
; (global-set-key "\C-xm" 'insert "\n M ")
(global-set-key "\C-xo" 'overwrite-mode)
; (global-set-key "\C-xx" 'insert "\n X ")

(global-set-key "\M-#" 'compile)
; (global-set-key "\M-\t" 'tab-to-tab-stop)
(global-set-key "\M-\t" 'c-indent-command)
(global-set-key "\M- " 'set-mark-command)
(global-set-key "\M--" 'copy-preceding-line)
(global-set-key "\M-{" 'c-stub)
(global-set-key "\M-[" 'backward-page)
(global-set-key "\M-]" 'forward-page)
(global-set-key "\M-(" 'start-kbd-macro)
(global-set-key "\M-)" 'end-kbd-macro)
(global-set-key "\M-`" 'compilation-next-error)
;(global-set-key "\M-d" 'global-word-downcase)
;(global-set-key "\M-e" 'append-entry-interactive)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-h" 'help-command)
(global-set-key "\M-j" 'diary-append-entry)
(global-set-key "\M-k" 'global-set-key)
(global-set-key "\M-r" 'comment-region)
(global-set-key "\M-s" 'save-some-buffers)
(global-set-key "\M-v" 'eval-current-buffer)
(global-set-key "\M-w" 'kill-region)

(global-set-key "\M-\C-?" 'delete-char)
(global-set-key "\M-\C-[" 'call-last-kbd-macro)
(global-set-key "\M-\C-b" 'beginning-of-line)
(global-set-key "\M-\C-f" 'find-file-read-only)
(global-set-key "\M-\C-r" 'query-replace-regexp)
(global-set-key "\M-\C-v" 'copy-preceding-line)
(font-lock-mode -1)

; (define-key view-mode-map "\C-w" 'other-window)
; (View-scroll-lines-forward-set-scroll-size 1)

;;;; ===============================================================
;;;; Function definitions
;;;; ===============================================================
;;; ---------------------------------------------------------------
;;; start up
;;; ---------------------------------------------------------------
(defun startup ()
  "Split the window and load the top level to do file"
  (interactive)
  (split-window-vertically)
  (dodo)
)

(put 'narrow-to-region 'disabled nil)
; (c-set-style "Ellemtel")

(setq filename (format "%s/el/pplog-mode.el" (getenv "HOME")))
(if (file-exists-p filename) (load-file filename))

(put 'set-goal-column 'disabled nil)
(display-time)


(put 'downcase-region 'disabled nil)

;; org mode stuff
(add-to-list 'load-path "~/Dropbox/prj/org-mode/lisp")
(add-to-list 'load-path "~/Dropbox/prj/org-mode/contrib/lisp")
(require 'org)

(add-to-list 'load-path "~/Dropbox/el")
(require 'org-journal)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(org-agenda-files (quote ("~/Dropbox/journal/personal.do" 
                            "~/Dropbox/journal/work.do")))
 '(org-ellipsis " >>>")
 '(org-log-done t)
 '(org-startup-folded 'nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(require 'org-checklist)
