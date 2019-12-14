(load-file "do-mode.el")
;;
;; This file contains tests for the functions in do-mode.el based on
;; the ERT framework documented at
;;
;;     https://www.emacswiki.org/emacs/ErtTestLibrary
;;
;; The ERT framework is described in more detail in emac's info
;; manual: esc-h i m ert
;;
;; Contents of this file
;;    variables
;;    helper functions
;;    do-add-done-iff                (1000 - 1020)
;;    bytes-at                       (1090 - 1099)
;;    do-done-position               (1100 - 1199)
;;    do-goto-next-task              (1200 - 1399)
;;    do-goto-prev-task              (1400 - 1599)
;;    do-new-task                    (1600 - 1699)
;;    do-next-task-mark              (1700 - 1799)
;;    do-prev-task-mark              (1800 - 1899)
;;    do-[pxo]done                   (1900 - 1999)
;;    do-buffer-p                    (2000 - 2099)
;;    do-task-up                     (2100 - 2199)
;;    do-task-down                   (2200 - 2299)
;;    {next,previous}-dodo           (2300 - 2399)
;;    do-task-to-top                 (2400 - 2499)
;;    do-task-to-end                 (2500 - 2599)


;; ============================================================================
;; variables
;; (setq g-1st "1st")
;;(setq g-2nd "2nd")
;; (setq g-3rd "3rd")
(setq g-d "d")
(setq g-h "-")
(setq g-m "m")
(setq g-n "n")
(setq g-o "<")
(setq g-p "+")
(setq g-w "w")
(setq g-x "x")
(setq g-nl "\n")
(setq g-sds " - ")
(setq g-sp " ")
(setq g-sls " < ")
(setq g-sps " + ")
(setq g-sxs " x ")
(setq g-rsps " \\+ ")
(setq g-trgx " [-+<x] ")
(setq g-task "task")
(setq g-sample "sample")
;; (setq g-1st-task (concat g-1st " " g-task))
;;(setq g-2nd-sample "2nd sample")
;;(setq g-2nd-task "2nd task")
;; (setq g-3rd-sample "3rd sample")
;; (setq g-3rd-task "3rd task")
(setq g-abandoned "\n\n x abandoned task number 2\n")
; (setq g-abandoned-1st "---\n\n x 1st")
(setq g-abc "abc")
(setq g-alphabet "abcdefghijklmnopqrstuvwxyz")
(setq g-also-sp "also ")
(setq g-artemis "artemis")
(setq g-both-do "both.do")
(setq g-first-task "first task")
(setq g-second-task "second task")
; (setq g-buf-no-done "      \n\n - first task\n - second task\n")
(setq g-6sp-2n "      \n\n")
(setq g-buf-w-done
      (concat "    \n\n\n"                ;  1 -  7
              " - task one\n\n"           ;  8 - 20
              " - task two  \n\n"         ; 21 - 35
              " - task three\n\n"         ; 36 - 50
              "--- DONE --------------------------------------------\n\n"
              " + finished 1\n\n"
              " + also done 2\n"))
(setq g-buf-samples1 (concat "\n\n - single sample task\n\n"))
(setq g-buf-samples2 (concat "\n\n - 1st sample task"
                             "\n\n - 2nd sample task"
                             "\n\n"))
(setq g-buf-samples3 (concat "\n\n - 1st sample task"
                             "\n\n - 2nd sample task"
                             "\n\n - 3rd sample task"
                             "\n\n"))
(setq g-completed "\n\n + completed task number 1")
(setq g-dash " - ")
(setq g-dash-1st " - 1st")
(setq g-dash-2nd " - 2nd")
(setq g-dash-3rd " - 3rd")
(setq g-dash-1st-sample "- 1st sample")
(setq g-dash-2nd-sample " - 2nd sample")
(setq g-dash-3rd-sample " - 3rd sample")
(setq g-dash-done "--- DONE ---")
(setq g-dash-first " - first")
(setq g-dash-singsamp " - single sample")
(setq g-dash-sp-f "- f")
(setq g-dash-sp-s "- s")
(setq g-dash-sp-t "- t")
(setq g-dash-tas " - tas")
(setq g-dash-task-thr " - task thr")
; (setq g-diverted-2nd "---\n\n < 2nd sample")
(setq g-done "DONE")
(setq g-done-line "--- DONE --------------------------------------------")
; (setq g-done-sample "---\n\n \\+ sample")
(setq g-e-new-new "e\n\n")
(setq g-empty "")
(setq g-file-too-small "file too small to hold a task")
(setq g-finis "finis")
(setq g-first "1st")
(setq g-hercules "hercules")
(setq g-inish "inish")
(setq g-k-sp-o "k o")
(setq g-less-1st-task "\n\n < 1st task")
(setq g-less-2nd-sample " < 2nd sample")
(setq g-less-3rd-sample " < 3rd sample")
(setq g-mnop "mnop")
;; (setq g-n "\n")
(setq g-n-e-sp-2-new "ne 2\n")
(setq g-new-new "\n\n")
(setq g-nn "\n\n")
; (setq g-new-new-sp "\n\n ")
(setq g-nn-sp "\n\n ")
(setq g-new2-dash "\n\n -")
(setq g-new3-dash "\n\n\n -")
(setq g-new-sp-dash "\n -")
(setq g-new-task-rgx " - \\[[.0-9]\\{9\\}\\] ")
(setq g-no-active-tasks "no active tasks found")
(setq g-no-tasks "no tasks in file")
(setq g-plus " + ")
(setq g-eplus " \\+ ")
(setq g-plus-1st " \\+ 1st")
(setq g-plus-1st-task "\n\n + 1st task\n\n")
(setq g-plus-1st-sample " \\+ 1st sample")
(setq g-plus-2nd " \\+ 2nd")
(setq g-plus-2nd-task "\n\n + 2nd task")
(setq g-plus-3rd-sample " \\+ 3rd sample")
(setq g-plus-3rd-task " \\+ 3rd task")
(setq g-plus-also "+ also")
(setq g-plus-completed " \\+ completed")
(setq g-plus-fini " + fini")
(setq g-plus-sample " \\+ sample")
(setq g-plus-single " \\+ single")
(setq g-plus-sp-f "+ f")
(setq g-plus-two-tasks "\n\n + 1st task\n\n + 2nd task\n\n")
(setq g-s-k-new "sk\n")
(setq g-sample "sample")
(setq g-sample-task "\n\n - sample task\n\n")
(setq g-second "2nd")
(setq g-six-new "\n\n\n\n\n\n")
(setq g-sp-also-sp " also ")
(setq g-sp-dash-sp " - ")
(setq g-sp-first " 1st")
(setq g-sp-plus-also " + also")
(setq g-sp-plus-sp " + ")
(setq g-sp-f-i " fi")
(setq g-sp-fini " fini")
(setq g-sp-new-new " \n\n")
(setq g-sp-s-e " se")
(setq g-sp-second " 2nd")
(setq g-sp-t-a " ta")
(setq g-sp-task " task")
(setq g-t-a-s "tas")
(setq g-tas "tas")
(setq g-t-sp-t "t t")
(setq g-task "task")
(setq g-three-new "\n\n\n")
(setq g-three-sp "   ")
(setq g-two-tasks "\n\n - 1st task\n\n - 2nd task\n\n")
(setq g-vwxyz "vwxyz")
(setq g-whitespace "       \n         \n              \n         ")
(setq g-x-first " x 1st")
(setq g-x-second-sample " x 2nd sample")
(setq g-xyz-do "xyz.do")
(setq g-msgbuf-name "*Messages*")

;; ============================================================================
;; helper functions

;; ----------------------------------------------------------------------------
(defun buffer-equal-p (str bufpos)
  "Return t if STR strictly matches (buffer-string) at BUFPOS,
otherwise nil. If STR extends past (point-max), the comparison
fails."
  (let ((result)
        (bufend (+ bufpos (length str)))
        )
    (if (< (point-max) bufend)
        (setq result nil)
      (setq result (string= str (buffer-substring bufpos bufend))))
    result
    ))

;; ----------------------------------------------------------------------------
(defun buffer-match-p (str bufpos)
  "Return t if STR matches (buffer-string) around BUFPOS,
otherwise nil. If str would extend past (point-max), it is
adjusted backward to attempt to match the end of the buffer."
  (string= str (bytes-at bufpos (length str)))
  )

;; ----------------------------------------------------------------------------
(defun buffer-pos (needle &optional offset)
  "Return 1-based position of NEEDLE in buffer plus OFFSET"
  (if (equal nil offset) (setq offset 0))
  (+ (string-match needle (buffer-string)) 1 offset))

;; ----------------------------------------------------------------------------
(defun get-message-max ()
 "Get the maximum size of the *Messages* buffer so we can track
subsequent messages."
  (with-current-buffer g-msgbuf-name
    (point-max)))

;; ----------------------------------------------------------------------------
(defun in-messages-p (after needle)
  "Find NEEDLE in buffer *Messages* following AFTER.

If NEEDLE is found return its index in the buffer substring
beginning at AFTER. If NEEDLE is not found, return nil."
  (with-current-buffer g-msgbuf-name
    (string-match needle (buffer-substring after (point-max)))))

;; ----------------------------------------------------------------------------
(defun in-order-p (&rest args)
  "Return t if items in list ARGS are in numeric order from lowest to highest"
  (catch 'bail
    (let ((a))
      (setq a (car args))
      (dolist (b (cdr args))
        (if (not (numberp b))
            (setq b (string-to-number b)))
        (if (< b a)
            (throw 'bail nil))
        (setq a b)
        ))
      t))

;; ----------------------------------------------------------------------------
(defun last-position (target &optional before)
  "Return the position of the last occurrence of TARGET in the buffer."
  (save-excursion
    (if before
        (goto-char before)
      (goto-char (point-max)))
    (re-search-backward target)))

;; ----------------------------------------------------------------------------
(defun run-tests ()
  "Set variable test-selector and return."
  (interactive)
  (ert-run-tests-batch-and-exit selector))

;; ----------------------------------------------------------------------------
(defun string-match-end (needle haystack)
  "If NEEDLE occurs in HAYSTACK, return the end of the match"
  (let ((where))
    (setq where (string-match needle haystack))
    (setq where (+ (length needle) where))
    ))

;; ----------------------------------------------------------------------------
(defun strmatch (needle haystack &optional offset)
  "Return (+ (string-match NEEDLE HAYSTACK) OFFSET).

If OFFSET is negative, the result will be before NEEDLE in
HAYSTACK. Note that string-match values are 0 based
while (buffer-string) and (buffer-substring ...) values are 1
based. Because of this, we add and extra 1 to adjust
string-match's return value to align it with buffer values."
  (let ((where))
    (if (equal offset nil)
        (setq offset 0))
    (setq where (+ 1 offset (string-match needle haystack)))
    where))

;; ----------------------------------------------------------------------------
(defun make-data-s (str)
  "Generate data based on contents of STR"
  (let ((rval "\n\n") (rlist ()) (tcount 1) (content) (mark))
    (dolist (item (split-string str))
      (if (string-match-p item "- \\+ < x")
          (progn (setq content (format "task %d" tcount))
                 (setq mark (concat " " item " "))
                 (setq tcount (+ 1 tcount))
                 (setq chunk (ftask content mark))
                 (setq rval (concat rval chunk "\n\n"))
                 )
        (if (string= "d" item)
            (setq rval (concat rval g-done-line "\n\n"))
          (if (string= "n" item)
              (setq rval (concat rval "\n"))
            (if (string= "m" item)
                (setq rval (concat rval "\n\n"))
              (if (string= "w" item)
                  (setq rval (concat rval "                    \n\n" ))))))))
    rval))

;; ----------------------------------------------------------------------------
(defun make-data (seq)
  "Generate data based on contents of SEQ"
  (let ((rval "") (tcount 1) (content) (mark))
    (dolist (item seq)
      (if (string-match-p item "- \\+ < x")
          (progn (setq content (format "task %d" tcount))
                 (setq mark (concat " " item " "))
                 (setq tcount (+ 1 tcount))
                 (setq rval (concat rval (ftask content mark 't))))
        (if (string= "d" item)
            (setq rval (concat rval g-done-line))
          (if (string= "n" item)
              (setq rval (concat rval "\n"))
            (if (string= "m" item)
                (setq rval (concat rval "\n\n"))
              (if (string= "w" item)
                  (setq rval (concat rval "                    "))))))))
    rval))

;; ----------------------------------------------------------------------------
(defun task (num &optional pfx)
  "Return the string 'task NUM'"
  (let ((ipfx "") (rval ""))
    (if pfx
        (setq rval pfx))
    (setq rval (concat rval (format "task %d" num)))
    rval))

;; ----------------------------------------------------------------------------
(defun ftask (&rest args)
  "Return a formatted task.

A task is comprised of a MARK followed by CONTENT. Here's an
exmample: ' - this is a task'. ' - ' is the MARK and the text is
the CONTENT. If CONTENT is not provided, the string 'example
task' is used. If MARK is not provided, the default ' - ' is
used. If POST is t, two newlines are appended. If PRE is t, two
newlines are prepended."
  (let ((content) (mark) (pre) (post))
    (if (not args)
        nil
      (setq content (car args))
      (setq args (cdr args))
      (if (not args)
          nil
        (setq mark (car args))
        (setq args (cdr args))
        (if (not args)
            nil
          (setq pre (car args))
          (setq args (cdr args))
          (if (not args)
              nil
            (setq post (car args))))))

    (if (equal mark nil) (setq mark " - "))
    (if (equal pre 't) (setq pre "\n\n"))
    (if (equal post 't) (setq post "\n\n"))
    (concat (if pre pre "")
            mark
            content
            (if post post ""))))


;; ----------------------------------------------------------------------------
;; more variables
(setq g-buf-no-done (concat (ftask g-first-task nil g-6sp-2n)
                            (ftask g-second-task nil g-nl 't)))


;; ============================================================================
;; tests for do-add-done-iff

;; ----------------------------------------------------------------------------
(ert-deftest test-1000-add-done-iff-empty ()
  "The payload function should add a done line if there isn't one
in the buffer already. In this test, the buffer is empty."
  (with-temp-buffer
    (do-add-done-iff)                   ; payload
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1010-add-done-iff-absent-w-tasks ()
  "Verify that do-add-done-iff adds a done line if there isn't
one in the buffer already. In this test, there are some tasks
in the buffer."
  (with-temp-buffer
    (let ((done-pos)
          (ltask-pos))
      (insert (make-data-s "- -"))
      (goto-char (point-min))
      (do-add-done-iff)                 ; payload
      (setq done-pos (do-done-position))
      (goto-char (point-max))
      (setq ltask-pos (re-search-backward do-mode-rgx-task))
      (should (not (equal nil done-pos)))
      (should (not (equal nil ltask-pos)))
      (should (< ltask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1020-add-done-iff-present ()
  "Verify that do-add-done-iff doesn't add a done line if one is
already present"
  (with-temp-buffer
    (insert (make-data-s "d n n n n"))
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))
    (do-add-done-iff)                 ; payload
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))
    (should (equal nil (re-search-forward do-mode-rgx-done nil 't)))))

;; ============================================================================
;; tests for bytes-at

;; ----------------------------------------------------------------------------
(ert-deftest test-1090-bytes-at-zero-min ()
  "bytes-at: zero-length buffer point min"
  (with-temp-buffer
    (should (string= g-empty (bytes-at (point-min) 3))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1091-bytes-at-zero-mid ()
  "bytes-at: zero-length buffer point middle"
  (with-temp-buffer
    (should (string= g-empty (bytes-at (point) 3))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1092-bytes-at-zero-max ()
  "bytes-at: zero-length buffer point max"
  (with-temp-buffer
    (should (string= g-empty (bytes-at (point-max) 3))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1093-bytes-at-short-min ()
  "bytes-at: short buffer point min"
  (with-temp-buffer
    (let ((source))
      (setq source (substring g-alphabet 0 2))
      (insert source)
      (should (string= source (bytes-at (point-min) 10))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1094-bytes-at-short-mid ()
  "bytes-at: short buffer point middle"
  (with-temp-buffer
    (let ((source))
      (setq source (substring g-alphabet 0 2))
      (insert source)
      (should (string= source (bytes-at 2 10))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1095-bytes-at-short-max ()
  "bytes-at: short buffer point max"
  (with-temp-buffer
    (let ((source))
      (setq source (substring g-alphabet 0 2))
      (insert source)
      (should (string= source (bytes-at (point-max) 10))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1096-bytes-at-long-min ()
  "bytes-at: long buffer point min"
  (with-temp-buffer
    (let ((exp (substring g-alphabet 0 3)))
      (insert g-alphabet)
      (should (string= exp (bytes-at (point-min) 3))) ; payload
    )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1097-bytes-at-long-mid ()
  "bytes-at: long buffer point middle"
  (with-temp-buffer
    (let ((exp (substring g-alphabet 12 16)))
      (insert g-alphabet)
      (should (string= exp (bytes-at 13 4))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1098-bytes-at-long-max ()
  "bytes-at: long buffer point max"
  (with-temp-buffer
    (let ((exp (substring g-alphabet -5)))
      (insert g-alphabet)
      (should (string= exp (bytes-at (point-max) 5))) ; payload
      )))

;; ============================================================================
;; tests for do-done-position

;; ----------------------------------------------------------------------------
(ert-deftest test-1100-do-done-position-no-done ()
  "test do-done-position when DONE line is missing"
  (with-temp-buffer
    (insert (make-data-s "n n n n n n"))
    (should (equal nil (do-done-position))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1110-do-done-position-with-done ()
  "test do-done-position when DONE line is present"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "d"))
      (setq exp (strmatch do-mode-rgx-done (buffer-string)))
      (should (= exp (do-done-position))) ; payload
      )))


;; ============================================================================
;; tests for do-goto-next-task

;; ----------------------------------------------------------------------------
(ert-deftest test-1210-next-no-done ()
  "goto-next: no DONE line, two tasks, point well before first task"
  (with-temp-buffer
    (let ((exp) (target))
      (insert (make-data-s "- -"))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (goto-char (strmatch (task 1) (buffer-string) 3))
      (setq target (do-goto-next-task)) ; payload
      (should (= exp target)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1215-next-no-done ()
  "goto-next: no DONE, two tasks, point just before first task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - n"))
      (goto-char (strmatch g-nn (buffer-string)))
      (setq exp (strmatch (task 1) (buffer-string) -3))
      (should (string= g-nn-sp (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1220-next-no-done ()
  "goto-next: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- -"))
      (goto-char (strmatch (task 1) (buffer-string) -3))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-sp-dash-sp (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1230-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- -"))
      (goto-char (strmatch (task 1) (buffer-string) -2))
      (should (string= g-dash-sp-t (bytes-at (point) 3)))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1240-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (let ((exp) (loc))
      (insert (make-data-s "- -"))
      (goto-char (strmatch (task 1) (buffer-string) -1))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-sp-task (bytes-at (point) 5)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1250-next-no-done ()
  "goto-next: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- -"))
      (goto-char (strmatch (task 1) (buffer-string)))
      (should (string= g-tas (bytes-at (point) 3)))
      (should (= (point) (strmatch g-tas (buffer-string))))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1255-next-no-done ()
  "goto-next: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (let ((exp) (loc))
      (insert (make-data-s "- -"))
      (goto-char (setq loc (strmatch (task 2) (buffer-string) -4)))
      (setq exp (+ 1 loc))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1260-next-no-done ()
  "goto-next: no DONE, two tasks, point at second task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- -"))
      (goto-char (setq exp (strmatch (task 2) (buffer-string) -3)))
      (should (string= g-sp-dash-sp (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1270-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside second task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- -"))
      (goto-char (setq exp (strmatch (task 2) (buffer-string) -2)))
      (should (string= g-dash-sp-t (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1280-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into second task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- -"))
      (goto-char (setq exp (strmatch (task 2) (buffer-string) -1)))
      (should (string= g-sp-task (bytes-at (point) (length g-sp-task))))
      (should (= (point) (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1290-next-no-done ()
  "goto-next: no DONE, two tasks, point near end of second task"
  (with-temp-buffer
    (insert (make-data-s "- -"))
    (goto-char (point-max))
    (should (buffer-match-p (concat (task 2) "\n\n") (point)))
    (should (= (point-max) (do-goto-next-task))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1310-next-w-done ()
  "goto-next: with DONE, four tasks, point at start of buffer"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (point-min))
      (should (string= g-nn-sp (bytes-at (point) 3)))
      (setq exp (strmatch (task 1) (buffer-string) -3))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1320-next-w-done ()
  "goto-next: with DONE, four tasks, point just before 1st task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch g-new-sp-dash (buffer-string)))
      (setq exp (+ 1 (point)))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1330-next-w-done ()
  "goto-next: with DONE, four tasks, point at 1st task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch g-sp-dash-sp (buffer-string)))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-sp-dash-sp (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1340-next-w-done ()
  "goto-next: with DONE, four tasks, point one byte into 1st task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 1) (buffer-string) -2))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-dash-sp-t (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1350-next-w-done ()
  "goto-next: with DONE, four tasks, point two bytes into 1st task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 1) (buffer-string) -1))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-sp-t-a (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1360-next-w-done ()
  "goto-next:  with DONE, four tasks, point in middle of 1st task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 1) (buffer-string) 3))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= (task 1) (bytes-at (- (point) 3) 6)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1370-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 2nd task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 2) (buffer-string) -4))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1380-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 2nd task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 2) (buffer-string) -3))
      (setq exp (strmatch (task 3) (buffer-string) -3))
      (should (string= g-sp-dash-sp (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1390-next-w-done ()
  "goto-next:  with DONE, four tasks, point a couple bytes into 2nd task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 2) (buffer-string)))
      (setq exp (strmatch (task 3) (buffer-string) -3))
      (should (string= (task 2) (bytes-at (point) 6)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1392-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 3rd task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 2) (buffer-string) 6))
      (setq exp (strmatch (task 3) (buffer-string) -3))
      (should (string= g-nn-sp (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1394-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 3rd task"
  (with-temp-buffer
    (let ((exp) (fin))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 3) (buffer-string) -4))
      (setq exp (strmatch (task 3) (buffer-string) -3))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (should (= exp (do-goto-next-task))) ; payload
      (setq fin (strmatch (task 4) (buffer-string) -3))
      (should (string= g-sp-plus-sp (bytes-at fin 3)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1396-next-w-done ()
  "goto-next:  with DONE, four tasks, point at end of 3rd task"
  (with-temp-buffer
    (let ((exp) (thr-nn "3\n\n"))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 3) (buffer-string) 5))
      (should (string= thr-nn (bytes-at (point) 3)))
      (setq exp (strmatch (task 4) (buffer-string) -3))
      (should (= exp (do-goto-next-task))) ; payload
      (should (string= g-sp-plus-sp (bytes-at exp 3)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1398-next-w-done ()
  "goto-next:  with DONE, four tasks, point in 4th (completed) task"
  (with-temp-buffer
    (let ((before) (exp) (plus-t "+ t"))
      (insert (make-data-s "- - - d + +"))
      (setq before (strmatch (task 4) (buffer-string) -3))
      (goto-char (+ 1 before))
      (should (string= plus-t (bytes-at (point) 3)))
      (setq exp (strmatch (task 5) (buffer-string) -3))
      (should (= exp (do-goto-next-task))) ; payload
      (should (string= g-sp-plus-sp (bytes-at before 3)))
      (should (string= g-sp-plus-sp (bytes-at exp 3)))
      )))

;; ============================================================================
;; tests for do-goto-prev-task
;;
;; We'll start at the bottom of the buffer and work backwards across
;; the DONE line if it's there

;; ----------------------------------------------------------------------------
(ert-deftest test-1400-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result) (exp)
          (t5-str " + task 5")
          (t5-rgx " \\+ task 5")
          (tail "ask 5\n\n"))
      (insert (make-data-s "- - - d + +"))
      (goto-char (point-max))
      (should (string= tail (bytes-at (point) (length tail))))
      (setq exp (strmatch t5-rgx (buffer-string)))
      (setq result (do-goto-prev-task)) ; payload
      (should (= exp result))
      (should (string= t5-str (bytes-at result (length t5-str))))
      )))


;; ----------------------------------------------------------------------------
(ert-deftest test-1405-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result) (t5-str " + task 5"))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 5) (buffer-string)))
      (setq exp (- (point) 3))
      (should (string= (task 5) (bytes-at (point) 6)))
      (setq result (do-goto-prev-task)) ; payload
      (should (= exp result))
      (should (string= t5-str (bytes-at result (length t5-str)))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1410-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task"
  (with-temp-buffer
    (let ((result) (sp-task-5 " task 5") (plus-task-4 " + task 4"))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 5) (buffer-string) -1))
      (should (string= sp-task-5 (bytes-at (point) (length sp-task-5))))
      (setq exp (strmatch (task 4) (buffer-string) -3))
      (setq result (do-goto-prev-task)) ; payload
      (should (= exp result))
      (should (string= plus-task-4 (bytes-at result (length plus-task-4))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1420-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task by 1 byte"
  (with-temp-buffer
    (let ((result)
          (plus-task-5 (concat (task 5 g-sps) g-nl))
          (pt4-rgx (task 4 g-rsps))
          (pt4-str (task 4 g-sps))
          )
      (goto-char (strmatch (task 5) (buffer-string) -2)) ; '+' before 'task 5'
      (should (string= plus-task-5 (bytes-at (point) (length plus-task-5))))
      (insert (make-data-s "- - - d + +"))
      (setq result (do-goto-prev-task)) ; payload
      (should (= result (strmatch pt4-rgx (buffer-string))))
      (should (string= pt4-str (bytes-at result (length pt4-str))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1430-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at last task"
  (with-temp-buffer
    (let ((result)
          (exp)
          (pt5 (task 5 g-sps))
          (pt4 (task 4 g-sps))
          )
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 5) (buffer-string) -3))
      (should (string= pt5 (bytes-at (point) (length pt5))))
      (setq result (do-goto-prev-task)) ; payload
      (setq exp (strmatch (task 4) (buffer-string) -3))
      (should (= exp result))
      (should (string= pt4 (bytes-at result (length pt4))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1440-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task"
  (with-temp-buffer
    (let ((result)
          (pt4 (task 4 g-sps))
          (ask4 "ask 4")
          )
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch ask4 (buffer-string)))
      (setq exp (strmatch (task 4) (buffer-string) -3))
      (setq result (do-goto-prev-task)) ; payload
      (should (= exp result))
      (should (string= pt4 (bytes-at result (length pt4))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1442-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point early in penultimate task"
  (with-temp-buffer
    (let ((result) (pt4 (task 4 g-sps)))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 4) (buffer-string)))
      (should (string= (task 4) (bytes-at (point) (length (task 4)))))
      (setq exp (strmatch (task 4) (buffer-string) -3))
      (setq result (do-goto-prev-task)) ; payload
      (should (= exp result))
      (should (string= pt4 (bytes-at result (length pt4))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1444-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task mark"
  (with-temp-buffer
    (let ((result) (st4 (task 4 " ")) (dt3 (task 3 g-sds)))
      (insert (make-data-s "- - - d + +"))
      (goto-char (strmatch (task 4) (buffer-string) -1))
      (should (string= st4 (bytes-at (point) (length st4))))
      (setq exp (strmatch (task 3) (buffer-string) -3))
      (setq result (do-goto-prev-task)) ; payload
      (should (= exp result))
      (should (string= dt3 (bytes-at result (length dt3))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1450-prev-w-done ()
  "prev task: with DONE, 5 tasks, point before 1st task, land on 1st"
  (with-temp-buffer
    (let ((result))
      (goto-char (strmatch (task 1) (buffer-string) -8))
      (should (string= g-new3-dash (bytes-at (point) (length g-new3-dash))))
      (setq exp (strmatch (task 1) (buffer-string) -3))
      (insert (make-data-s "n - - - d + +"))
      (setq result (do-goto-prev-task)) ; payload
      (should (= exp result))
      (should (string= g-dash-tas (bytes-at result (length g-dash-tas))))
      )))

;; ============================================================================
;; tests for do-new-entry
;;

;; ----------------------------------------------------------------------------
(ert-deftest test-1600-new-001 ()
  "new entry: empty file"
  (with-temp-buffer
    (do-new-task)                       ; payload
    (should (string-match g-new-task-rgx (buffer-string)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1605-new-002 ()
  "new entry: no DONE line, one task, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (otask-pos) (task-1 (task 1)))
      (insert (make-data-s "-"))
      (goto-char (point-min))
      (do-new-task)                     ; payload
      (goto-char (point-min))
      (should (setq ntask-pos (strmatch g-new-task-rgx (buffer-string))))
      (should (setq otask-pos (strmatch task-1 (buffer-string) -3)))
      (should (< ntask-pos otask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1610-new-003 ()
  "new entry: no DONE line, one task, after 1st"
  (with-temp-buffer
    (let ((ntask-pos) (otask-pos))
      (insert (make-data-s "-"))
      (goto-char (string-match g-task (buffer-string)))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq otask-pos (string-match (task 1) (buffer-string))))
      (should (< otask-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1615-new-004 ()
  "new entry: no DONE line, two tasks, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert (make-data-s "- -"))
      (goto-char (point-min))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (strmatch (task 1) (buffer-string) -3)))
      (should (setq second-pos (strmatch (task 2) (buffer-string) -3)))
      (should (in-order-p ntask-pos first-pos second-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1620-new-005 ()
  "new entry: no DONE line, two tasks, before 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert (make-data-s "- -"))
      (goto-char (strmatch (task 2) (buffer-string) -5))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (strmatch g-new-task-rgx (buffer-string) -3)))
      (should (setq first-pos (strmatch (task 1) (buffer-string) -3)))
      (should (setq second-pos (strmatch (task 2) (buffer-string) -3)))
      (should (in-order-p first-pos ntask-pos second-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1625-new-006 ()
  "new entry: no DONE line, two tasks, after 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert (make-data-s "- -"))
      (goto-char (- (point-max) 1))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (strmatch g-new-task-rgx (buffer-string) -3)))
      (should (setq first-pos (strmatch (task 1) (buffer-string) -3)))
      (should (setq second-pos (strmatch (task 2) (buffer-string) -3)))
      (should (in-order-p first-pos second-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1630-new-007 ()
  "new entry: no DONE line, three tasks, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert (make-data-s "- - -"))
      (goto-char 2)
      (do-new-task)                     ; payload
      (should (setq ntask-pos (strmatch g-new-task-rgx (buffer-string))))
      (should (setq first-pos (strmatch (task 1) (buffer-string) -3)))
      (should (setq second-pos (strmatch (task 2) (buffer-string) -3)))
      (should (setq third-pos (strmatch (task 3) (buffer-string) -3)))
      (should (< ntask-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1635-new-008 ()
  "new entry: no DONE line, three tasks, before 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert (make-data-s "- - -"))
      (goto-char (strmatch (task 1) (buffer-string)))
      (end-of-line)
      (do-new-task)                     ; payload
      (should (setq ntask-pos (strmatch g-new-task-rgx (buffer-string))))
      (should (setq first-pos (strmatch (task 1) (buffer-string) -3)))
      (should (setq second-pos (strmatch (task 2) (buffer-string) -3)))
      (should (setq third-pos (strmatch (task 3) (buffer-string) -3)))
      (should (in-order-p first-pos ntask-pos second-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1640-new-009 ()
  "new entry: no DONE line, three tasks, before 3rd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert (make-data-s "- - -"))
      (goto-char (strmatch (task 2) (buffer-string) 5))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (strmatch g-new-task-rgx (buffer-string))))
      (should (setq first-pos (strmatch (task 1) (buffer-string) -3)))
      (should (setq second-pos (strmatch (task 2) (buffer-string) -3)))
      (should (setq third-pos (strmatch (task 3) (buffer-string) -3)))
      (should (in-order-p second-pos ntask-pos third-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1645-new-010 ()
  "new entry: no DONE line, three tasks, after 3rd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert (make-data-s "- - -"))
      (goto-char (strmatch (task 3) (buffer-string) 5))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (strmatch g-new-task-rgx (buffer-string))))
      (should (setq first-pos (strmatch (task 1) (buffer-string) -3)))
      (should (setq second-pos (strmatch (task 2) (buffer-string) -3)))
      (should (setq third-pos (strmatch (task 3) (buffer-string) -3)))
      (should (< third-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1650-new-011 ()
  "new entry: DONE line present, no tasks"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert (make-data-s "d"))
      (goto-char (point-min))
      (do-new-task)                     ; payload
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match g-new-task-rgx (buffer-string)))
      (should (< ntask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1655-new-011 ()
  "new entry: DONE line present, no tasks"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert (make-data-s "d"))
      (goto-char (point-max))
      (do-new-task)                     ; payload
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match g-new-task-rgx (buffer-string)))
      (should (< ntask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1660-new-012 ()
  "new entry: DONE line present, one task, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert (make-data-s "- d"))
      (goto-char (point-min))
      (do-new-task)                     ; payload
      (setq done-pos (do-done-position))
      (setq ntask-pos (strmatch g-new-task-rgx (buffer-string)))
      (setq first-pos (strmatch (task 1) (buffer-string) -3))
      (should (in-order-p ntask-pos first-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1665-new-012 ()
  "new entry: DONE line present, one task, after 1st"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos) (first-pos))
      (insert (make-data-s "- d"))
      (goto-char (strmatch g-task (buffer-string)))
      (do-new-task)                     ; payload
      (setq done-pos (do-done-position))
      (setq ntask-pos (strmatch g-new-task-rgx (buffer-string)))
      (setq first-pos (strmatch g-task (buffer-string) -3))
      (should (in-order-p first-pos ntask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1670-new-012 ()
  "new entry: DONE line present, one task, after done line"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert (make-data-s "- m d m"))
      (goto-char (point-max))
      (do-new-task)                     ; payload
      (setq done-pos (do-done-position))
      (setq ntask-pos (strmatch g-new-task-rgx (buffer-string)))
      (setq first-pos (strmatch (task 1) (buffer-string) -3))
      (should (in-order-p first-pos ntask-pos done-pos))
      )))

;; ============================================================================
;; tests for do-next-task-mark
;;

;; ----------------------------------------------------------------------------
(ert-deftest test-1700-ntm-no-done ()
  "next-task: no DONE line, two tasks, point well before first task"
  (with-temp-buffer
    (let ((task1-pos) (dt1 (task 1 g-sds)))
      (insert (make-data-s "- - m"))
      (setq task1-pos (strmatch (task 1) (buffer-string) -3))
      (goto-char 2)
      (should (= task1-pos (do-next-task-mark))) ; payload
      (should (string= dt1 (bytes-at task1-pos (length dt1))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1703-ntm-no-done ()
  "next-task: no DONE, two tasks, point just before first task"
  (with-temp-buffer
    (let ((result) (exp) (dt1 (task 1 g-sds)))
      (insert (make-data-s "- - m"))
      (goto-char (strmatch (task 1) (buffer-string) -5))
      (should (string= g-nn-sp (bytes-at (point) (length g-nn-sp))))
      (setq exp (strmatch (task 1) (buffer-string) -3))
      (setq result (do-next-task-mark)) ; payload
      (should (= exp result))
      (should (string= dt1 (bytes-at result (length dt1))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1706-ntm-no-done ()
  "next-task: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - m"))
      (goto-char (strmatch (task 1) (buffer-string) -3))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-sp-dash-sp (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1709-ntm-no-done ()
  "next-task: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - m"))
      (goto-char (strmatch (task 1) (buffer-string) -2))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-dash-sp-t (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1712-ntm-no-done ()
  "next-task: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (insert (make-data-s "- - m"))
    (goto-char (strmatch (task 1) (buffer-string) -1))
    (setq exp (strmatch (task 2) (buffer-string) -3))
    (should (string= g-sp-t-a (bytes-at (point) 3)))
    (should (= exp (do-next-task-mark))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1715-ntm-no-done ()
  "next-task: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (let ((exp) (t1 (task 1)) (k-sp-1 (substring (task 1) 3 6)))
      (insert (make-data-s "- - m"))
      (goto-char (strmatch (task 1) (buffer-string) 3))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= k-sp-1 (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1718-ntm-no-done ()
  "next-task: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - m"))
      (goto-char (strmatch (task 1) (buffer-string) 7))
      (setq exp (strmatch (task 2) (buffer-string) -3))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1721-ntm-no-done ()
  "next-task: no DONE, two tasks, from '[ ]- task 2', payload returns nil"
  (with-temp-buffer
    (insert (make-data-s "- - m"))
    (goto-char (strmatch (task 2) (buffer-string) -3))
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1724-ntm-no-done ()
  "next-task: no DONE, two tasks, from ' [-] task 2', payload returns nil"
  (with-temp-buffer
    (insert (make-data-s "- - m"))
    (goto-char (strmatch (task 2) (buffer-string) -2))
    (should (string= g-dash-sp-t (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1727-ntm-no-done ()
  "next-task: no DONE, two tasks, from ' -[ ]task 2', payload returns nil"
  (with-temp-buffer
    (insert (make-data-s "- - m"))
    (goto-char (strmatch (task 2) (buffer-string) -1))
    (should (string= g-sp-t-a (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1730-ntm-no-done ()
  "next-task: no DONE, two tasks, from ' - tas[k] 2', payload returns nil"
  (with-temp-buffer
    (insert (make-data-s "- - m"))
    (goto-char (strmatch (task 2) (buffer-string) 3))
    (should (equal nil (do-next-task-mark))) ; payload
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1733-ntm-w-done ()
  "next-task: with DONE, five tasks, point at start of buffer,
find 'task 1'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (point-min))
      (setq exp (buffer-pos (task 1) -3))
      (should (string= g-nl (bytes-at (point) 1)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1736-ntm-w-done ()
  "next-task: with DONE, five tasks, with point just before 'task 1',
payload finds 'task 1'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 1) -4))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (setq exp (buffer-pos (task 1) -3))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1739-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at 'task 1',
payload finds 'task 2'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 1) -3))
      (should (string= g-sp-dash-sp (bytes-at (point) 3)))
      (setq exp (buffer-pos (task 2) -3))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1742-ntm-w-done ()
  "next-task: with DONE, five tasks, with point on '- task 1',
payload finds ' - task 2'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 1) -2))
      (setq exp (buffer-pos (task 2) -3))
      (should (string= g-dash-sp-t (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1745-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at ' task 1',
payload finds ' - task 2'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 1) -1))
      (setq exp (buffer-pos (task 2) -3))
      (should (string= g-sp-t-a (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1748-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at 'k 1', payload
finds ' - task 2'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 1) 3))
      (setq exp (buffer-pos (task 2) -3))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1751-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at '\n - task 2',
payload finds ' - task 2'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 2) -4))
      (setq exp (buffer-pos (task 2) -3))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1754-ntm-w-done ()
  "next-task: with DONE, five tasks, point at ' - task 2',
payload finds ' - task 3'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 2) -3))
      (setq exp (buffer-pos (task 3) -3))
      (should (string= g-sp-dash-sp (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1757-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at 'task 2',
payload finds ' - task 3'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 2)))
      (setq exp (buffer-pos (task 3) -3))
      (should (string= g-t-a-s (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1760-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at '\n\n - task
3', payload finds ' - task 3'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos (task 3) -3))
      (goto-char (- exp 2))
      (should (string= "\n\n " (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1763-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at '\n - task 3',
payload finds ' - task 3'"
  (with-temp-buffer
    (let ((exp))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos (task 3) -3))
      (goto-char (- exp 1))
      (should (string= g-new-sp-dash (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1766-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at 'task >3<\n ',
payload finds ' + task 4'"
  (with-temp-buffer
    (let ((exp) (t3-tail "3\n\n"))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos (task 4) -3))
      (goto-char (buffer-pos (task 3) 5))
      (should (string= t3-tail (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1769-ntm-w-done ()
  "next-task: with DONE, five tasks, with point at '>+< task 4',
payload finds ' + task 5'"
  (with-temp-buffer
    (let ((exp) (plus-sp-t "+ t"))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 4) -2))
      (setq exp (buffer-pos (task 5) -3))
      (should (string= plus-sp-t (bytes-at (point) 3)))
      (should (= exp (do-next-task-mark))) ; payload
      )))

;; ============================================================================
;; tests for do-prev-task-mark
;;

;; ----------------------------------------------------------------------------
(ert-deftest test-1800-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from (point-max),
payload finds ' + task 5'"
  (with-temp-buffer
    (let ((result)
          (task-5 (concat (task 5) "\n\n"))
          (p-task-5 (concat g-sps (task 5))))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos task-5 -3))
      (goto-char (point-max))
      (should (buffer-match-p task-5 (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= result exp))
      (should (buffer-equal-p p-task-5 result))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1805-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from '+ task[ ]5', payload
finds ' + task 5'"
  (with-temp-buffer
    (let ((result)
          (exp)
          (s-task-5 " 5")
          (exp-task-5 (concat g-sps (task 5)))
          )
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 5) 4))
      (setq exp (buffer-pos (task 5) -3))
      (should (buffer-equal-p s-task-5 (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= result exp))
      (should (buffer-equal-p exp-task-5 result))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1810-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from ' + t[a]sk 5', payload
finds ' + task 5'"
  (with-temp-buffer
    (let ((result)
          (ask-5 "ask 5")
          (p-task-5 (concat g-sps (task 5))))
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 5) +1))
      (setq exp (buffer-pos (task 5) -3))
      (should (buffer-equal-p ask-5 (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= exp result))
      (should (buffer-equal-p p-task-5 result))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1815-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from ' + [t]ask 5', payload finds ' + task 5'"
  (with-temp-buffer
    (let ((result)
          (p-task-5 (concat g-sps (task 5)))
          (exp))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos (task 5) -3))
      (goto-char (buffer-pos (task 5)))
      (should (buffer-equal-p (task 5) (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= exp result))
      (should (buffer-equal-p p-task-5 result))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1820-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from '[ ]+ task 5', payload finds ' + task 4'"
  (with-temp-buffer
    (let ((result)
          (p-task-4 (concat g-sps (task 4)))
          (p-task-5 (concat g-sps (task 5)))
          )
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 5) -3))
      (should (buffer-equal-p p-task-5 (point)))
      (setq exp (buffer-pos (task 4) -3))
      (setq result (do-prev-task-mark)) ; payload
      (should (= exp result))
      (should (buffer-equal-p p-task-4 result)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1825-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from '[a]sk 4', payload finds ' + task 4'"
  (with-temp-buffer
    (let ((result)
          (ask-4 "ask 4")
          (p-task-4 (concat g-sps (task 4)))
          (exp)
          )
      (insert (make-data-s "- - - n d + +"))
      (goto-char (buffer-pos (task 4) 1))
      (setq exp (buffer-pos (task 4) -3))
      (should (buffer-equal-p ask-4 (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= result exp))
      (should (buffer-equal-p p-task-4 result))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1830-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from '[t]ask 4', payload finds ' - task 4'"
  (with-temp-buffer
    (let ((result))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos (task 4) -3))
      (goto-char (buffer-pos (task 4)))
      (should (buffer-equal-p (task 4) (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= result exp))
      (should (buffer-equal-p (task 4 g-sps) result))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1835-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, from '[]task 4', payload finds ' - task 4'"
  (with-temp-buffer
    (let ((result)
          (exp))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos (task 3 g-sds)))
      (goto-char (buffer-pos (task 4) -1))
      (should (buffer-equal-p (task 4 g-sp) (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= result exp))
      (should (buffer-equal-p (task 3 g-sds) result))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1840-ptm-w-done ()
  "prev task: with DONE, 5 tasks, from '[\n] - task 1', payload
finds ' - task 1'"
  (with-temp-buffer
    (let ((result)
          (nl-d-task-1 (task 1 "\n - "))
          (exp))
      (insert (make-data-s "- - - n d + +"))
      (setq exp (buffer-pos (task 1) -3))
      (goto-char (buffer-pos (task 1) -4))
      (should (buffer-equal-p nl-d-task-1 (point)))
      (setq result (do-prev-task-mark)) ; payload
      (should (= result exp))
      (should (buffer-equal-p (task 1 g-sds) result))
      )))

;; ============================================================================
;; tests for do-[pxo]done

;; ----------------------------------------------------------------------------
(ert-deftest test-1900-pdone-empty ()
  "pdone: empty file doesn't change"
  (with-temp-buffer
    (let ((msg-max (get-message-max)))
      (do-pdone 't)                     ; payload
      (should (in-messages-p msg-max g-file-too-small))
      (should (= (point-max) 1))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1905-xdone-whitespace ()
  "xdone: file of whitespace doesn't change"
  (with-temp-buffer
    (let ((msg-max (get-message-max))
          (before)
          (pre-point))
      (insert (make-data-s "w n w n w n w"))
      (setq before (buffer-string))
      (setq pre-point (point))
      (do-xdone 't)                     ; payload
      (should (in-messages-p msg-max g-no-tasks))
      (should (string= before (buffer-string)))
      (should (= pre-point (point)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1910-odone-no-active ()
  "odone: no active tasks: no changes, message"
  (with-temp-buffer
    (let ((msg-max (get-message-max))
          (before)
          (pre-point))
      (insert (make-data-s "d + x"))
      (setq before (buffer-string))
      (goto-char (point-max))
      (setq pre-point (point))
      (do-odone 't)                     ; payload
      (should (in-messages-p msg-max g-no-active-tasks))
      (should (string= before (buffer-string)))
      (should (= pre-point (point)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1915-pdone-ndl-one ()
  "pdone: one task, no DONE -> DONE line added, task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert (make-data-s "- n"))
      (goto-char (buffer-pos (task 1)))
      (do-pdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (buffer-pos (task 1) -3))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position (task 1 g-rsps))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1920-xdone-ndl-two-1st ()
  "two task, no DONE: DONE line added, first task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-1-pos)
          (task-2-pos)
          )
      (insert (make-data-s "- - n"))
      (goto-char (buffer-pos (task 1 g-sp)))
      (do-xdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-1-pos (last-position do-mode-rgx-task))
      (setq task-2-pos (buffer-pos (task 2) -3))
      (should (in-order-p task-2-pos done-pos task-1-pos))
      (should (= task-1-pos (last-position (task 1 g-sxs))))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1925-odone-ndl-two-2nd ()
  "two task, no DONE: DONE line added, second task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-1-pos)
          (task-2-pos))
      (insert (make-data-s "- - n"))
      (goto-char (buffer-pos (task 2) -1))
      (do-odone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq task-1-pos (buffer-pos (task 1 g-trgx)))
      (setq task-2-pos (buffer-pos (task 2 g-trgx)))
      (should (in-order-p task-1-pos done-pos task-2-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1930-pdone-ndl-three-1st ()
  "three tasks, no DONE: DONE line added, first task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-1-pos)
          (task-2-pos)
          (task-3-pos)
          )
      (insert (make-data-s "- - - n"))
      (goto-char (buffer-pos (task 1) -3))
      (do-pdone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq task-1-pos (buffer-pos (task 1 g-trgx)))
      (setq task-2-pos (buffer-pos (task 2 g-trgx)))
      (setq task-3-pos (buffer-pos (task 3 g-trgx)))
      (should (in-order-p task-2-pos task-3-pos done-pos task-1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1935-xdone-ndl-three-2nd ()
  "three tasks, no DONE: DONE line added, middle task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-1-pos)
          (task-2-pos)
          (task-3-pos)
          )
      (insert (make-data-s "- - - n"))
      (goto-char (buffer-pos (task 2)))
      (do-xdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-1-pos (buffer-pos (task 1 g-trgx)))
      (setq task-2-pos (buffer-pos (task 2 g-trgx)))
      (setq task-3-pos (buffer-pos (task 3 g-trgx)))
      (should (in-order-p task-1-pos task-3-pos done-pos task-2-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1940-odone-ndl-three-3rd ()
  "three tasks, no DONE: DONE line added, last task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-1-pos)
          (task-2-pos)
          (task-3-pos))
      (insert (make-data-s "- - - n"))
      (goto-char (buffer-pos (task 3)))
      (do-odone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq task-1-pos (buffer-pos (task 1) -3))
      (setq task-2-pos (buffer-pos (task 2) -3))
      (setq task-3-pos (buffer-pos (task 3) -3))
      (should (in-order-p task-1-pos task-2-pos done-pos task-3-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1945-pdone-wdl-one ()
  "one task, with DONE: task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert (make-data-s "- n d"))
      (goto-char (buffer-pos (task 1)))
      (do-pdone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (buffer-pos (task 1) -3))
      (should (< done-pos task-pos))
      (should (buffer-equal-p (task 1 g-sps) task-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1950-xdone-wdl-two-1st ()
  "two tasks, with DONE: first task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert (make-data-s "- - n d"))
      (goto-char (buffer-pos (task 1)))
      (do-xdone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq task-1-pos (buffer-pos (task 1) -3))
      (setq task-2-pos (buffer-pos (task 2) -3))
      (should (in-order-p task-2-pos done-pos task-1-pos))
      (should (buffer-equal-p (task 1 g-sxs) task-1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1955-odone-wdl-two-2nd ()
  "two tasks, with DONE: second task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-1-pos) (task-2-pos))
      (insert (make-data-s "- - n d"))
      (goto-char (buffer-pos (task 2)))
      (do-odone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq task-1-pos (buffer-pos (task 1) -3))
      (setq task-2-pos (buffer-pos (task 2) -3))
      (should (in-order-p task-1-pos done-pos task-2-pos))
      (should (buffer-equal-p (task 2 g-sls) task-2-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1960-pdone-wdl-three-1st ()
  "three tasks, with DONE: 1st task moves below"
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos) (t3-pos))
      (insert (make-data-s "- - - n d"))
      (goto-char (buffer-pos (task 1) -1))
      (do-pdone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq t3-pos (buffer-pos (task 3) -3))
      (should (in-order-p t2-pos t3-pos done-pos t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1965-xdone-wdl-three-2nd ()
  "three tasks, with DONE: 2nd task moves below"
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos) (t3-pos))
      (insert (make-data-s "- - - n d"))
      (goto-char (buffer-pos (task 2)))
      (do-xdone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq t3-pos (buffer-pos (task 3) -3))
      (should (in-order-p t1-pos t3-pos done-pos t2-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1970-odone-wdl-three-3rd ()
  "three tasks, with DONE: 3rd task moves below"
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos) (t3-pos))
      (insert (make-data-s "- - - n d"))
      (goto-char (buffer-pos (task 3) -2))
      (do-odone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq t3-pos (buffer-pos (task 3) -3))
      (should (in-order-p t1-pos t2-pos done-pos t3-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1975-odone-wdl-three-3rd ()
  "three tasks, with DONE: 2nd, 3rd tasks move below DONE"
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos) (t3-pos))
      (insert (make-data-s "- - - n d"))
      (goto-char (buffer-pos (task 2) -2))
      (do-odone 't)                     ; payload
      (do-pdone 't)                     ; payload
      (setq done-pos (do-done-position))
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq t3-pos (buffer-pos (task 3) -3))
      (should (in-order-p t1-pos done-pos t3-pos t2-pos))
      )))

;; ============================================================================
;; tests for do-buffer-p

;; ----------------------------------------------------------------------------
(ert-deftest test-2000-dobufp-cur-no ()
  "current buffer is not a do-buffer"
  (with-temp-buffer
    (should (equal nil (do-buffer-p)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-2005-dobufp-cur-yes ()
  "current buffer *is* a do-buffer"
  (with-temp-buffer
    (do-mode)
    (should (do-buffer-p))))

;; ----------------------------------------------------------------------------
(ert-deftest test-2010-dobufp-other-no ()
  "other buffer is not a do-buffer"
  (let ((bufname g-artemis))
    (create-file-buffer bufname)
    (should (not (do-buffer-p bufname))) ; payload
    (kill-buffer bufname)))

;; ----------------------------------------------------------------------------
(ert-deftest test-2015-dobufp-other-yes-mode ()
  "other buffer *is* a do-buffer by mode"
  (let ((bufname g-hercules))
    (create-file-buffer bufname)
    (with-current-buffer bufname
      (do-mode))
    (should (do-buffer-p bufname))      ; payload
    (kill-buffer bufname)))

;; ----------------------------------------------------------------------------
(ert-deftest test-2020-dobufp-other-yes-name ()
  "other buffer is a do-buffer by name"
  (let ((bufname g-xyz-do))
    (create-file-buffer bufname)
    (should (do-buffer-p bufname))      ; payload
    (kill-buffer bufname)
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-2020-dobufp-other-yes-both ()
  "other buffer is a do-buffer by both mode and name"
  (let ((bufname g-both-do))
    (create-file-buffer bufname)
    (with-current-buffer bufname
      (do-mode))
    (should (do-buffer-p bufname))      ; payload
    (kill-buffer bufname)
    ))

;; ============================================================================
;; tests for do-task-up

;; ----------------------------------------------------------------------------
(ert-deftest test-2100-do-task-up-zlen ()
  "do-task-up: zero length buffer -- do nothing"
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-up)                      ; payload
      (should (buffer-equal-p before (point-min)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2105-do-task-up ()
  "do-task-up: whitespace -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "w"))
      (setq before (buffer-string))
      (goto-char (floor (point-max) 2))
      (do-task-up)                      ; payload
      (should (buffer-equal-p before (point-min)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2110-do-task-up ()
  "do-task-up: one task, no DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "-"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-up)                      ; payload
      (should (buffer-equal-p before (point-min)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2115-do-task-up ()
  "do-task-up: one task, above DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- m d"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-up)                      ; payload
      (should (buffer-equal-p before (point-min)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2120-do-task-up ()
  "do-task-up: one task, above DONE -- moving DONE doesn't do anything"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- m d"))
      (setq before (buffer-string))
      (goto-char (buffer-pos g-done))
      (do-task-up)                      ; payload
      (should (buffer-equal-p before (point-min)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2125-do-task-up ()
  "do-task-up: one task, below DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "d +"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-up)                      ; payload
      (should (buffer-equal-p before (point-min)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2130-do-task-up ()
  "do-task-up: two tasks, no DONE -- top doesn't move"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - m"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2135-do-task-up ()
  "do-task-up: two tasks, no DONE -- bottom moves past top"
  (with-temp-buffer
    (let ((t1-pos) (t2-pos))
      (insert (make-data-s "- - m"))
      (goto-char (buffer-pos (task 2)))
      (do-task-up)                      ; payload
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (should (equal nil (do-done-position)))
      (should (< t2-pos t1-pos))
      (should (= (point) t2-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2140-do-task-up ()
  "do-task-up: two tasks, above DONE -- bottom moves past top"
  (with-temp-buffer
    (let ((t1-pos) (t2-pos) (done-pos))
      (insert (make-data-s "- - n d"))
      (goto-char (buffer-pos (task 2)))
      (do-task-up)                      ; payload
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq done-pos (do-done-position))
      (should (in-order-p t2-pos t1-pos done-pos))
      (should (= (point) t2-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2145-do-task-up ()
  "do-task-up: two tasks, above DONE -- DONE won't move past bottom"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - n d"))
      (setq before (buffer-string))
      (goto-char (+ 5 (do-done-position)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2150-do-task-up ()
  "do-task-up: two tasks, below DONE -- bottom moves past top"
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos))
      (insert (make-data-s "d + + m"))
      (goto-char (buffer-pos (task 2) 3))
      (do-task-up)                      ; payload
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq done-pos (do-done-position))
      (should (in-order-p done-pos t2-pos t1-pos))
      (should (= t2-pos (point)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2155-do-task-up ()
  "do-task-up: two tasks, below DONE -- top won't move past DONE"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "d + + m"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1) 3))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2160-do-task-up ()
  "do-task-up: three tasks, DONE after 2 -- upping 2nd should not
take DONE with it"
  (with-temp-buffer
    (let ((t1-pos) (t2-pos) (t3-pos) (done-pos))
      (insert (make-data-s "- - m d + n"))
      (goto-char (buffer-pos (task 2) 3))
      (do-task-up)                      ; payload
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq t3-pos (buffer-pos (task 3) -3))
      (setq done-pos (do-done-position))
      (should (in-order-p t2-pos t1-pos done-pos t3-pos))
      (should (= (point) t2-pos))
      )))

;; ============================================================================
;; tests for do-task-down

;; ----------------------------------------------------------------------------
(ert-deftest test-2200-do-task-down-zlen ()
  "do-task-down: zero length buffer -- do nothing"
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2205-do-task-down ()
  "do-task-down: whitespace -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "w"))
      (setq before (buffer-string))
      (goto-char (floor (point-max) 2))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2210-do-task-down ()
  "do-task-down: one task, no DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "+ m"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1) -3))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2215-do-task-down ()
  "do-task-down: one task, above DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "+ m d"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2220-do-task-down ()
  "do-task-down: one task, above DONE -- moving DONE doesn't do anything"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "+ m d"))
      (setq before (buffer-string))
      (goto-char (buffer-pos g-done))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2225-do-task-down ()
  "do-task-down: one task, below DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "d +"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2230-do-task-down ()
  "do-task-down: two tasks, no DONE -- bottom doesn't move"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - m"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 2)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2235-do-task-down ()
  "do-task-down: two tasks, no DONE -- top moves past bottom"
  (with-temp-buffer
    (let ((t1-pos) (t2-pos))
      (insert (make-data-s "- - m"))
      (goto-char (buffer-pos (task 1)))
      (do-task-down)                    ; payload
      (setq t2-pos (buffer-pos (task 2) -3))
      (setq t1-pos (buffer-pos (task 1) -3))
      (should (equal nil (do-done-position)))
      (should (< t2-pos t1-pos))
      (should (= (point) t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2240-do-task-down ()
  "do-task-down: two tasks, above DONE -- top moves past bottom"
  (with-temp-buffer
    (let ((t1-pos) (t2-pos) (done-pos))
      (insert (make-data-s "- - m d"))
      (goto-char (buffer-pos (task 1)))
      (do-task-down)                    ; payload
      (setq t2-pos (buffer-pos (task 2 g-sds)))
      (setq t1-pos (buffer-pos (task 1 g-sds)))
      (setq done-pos (do-done-position))
      (should (in-order-p t2-pos t1-pos done-pos))
      (should (= (point) t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2245-do-task-down ()
  "do-task-down: two tasks, below DONE -- DONE won't move past
the task after it"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "d < + m"))
      (setq before (buffer-string))
      (goto-char (+ 5 (do-done-position)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2250-do-task-down ()
  "do-task-down: two tasks, below DONE -- top moves past bottom"
  (with-temp-buffer
    (let ((before) (t1-pos) (t2-pos))
      (insert (make-data-s "d + + m"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-down)                    ; payload
      (setq t1-pos (buffer-pos (task 1) -3))
      (setq t2-pos (buffer-pos (task 2) -3))
      (should (in-order-p (do-done-position) t2-pos t1-pos))
      (should (= (point) t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2255-do-task-down ()
  "do-task-down: two tasks, above DONE -- bottom won't move past DONE"
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - m d"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 2)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2260-do-task-down ()
  "do-task-down: three tasks, DONE after 1 -- downing 2nd should
not take DONE with it"
  (with-temp-buffer
    (let ((t1-pos) (t2-pos) (t3-pos) (done-pos))
      (insert (make-data-s "- m d + +"))
      (goto-char (buffer-pos (task 2) 5))
      (do-task-down)                    ; payload
      (setq t1-pos (buffer-pos (task 1 g-sds)))
      (setq t2-pos (buffer-pos (task 2 g-rsps)))
      (setq t3-pos (buffer-pos (task 3 g-rsps)))
      (setq done-pos (do-done-position))
      (should (in-order-p t1-pos done-pos t3-pos t2-pos))
      (should (= (point) t2-pos))
      )))

;; ============================================================================
;; tests for next-dodo, previous-dodo

;; ----------------------------------------------------------------------------
(ert-deftest test-2300-next-dodo ()
  "next-dodo: jump from one dodo buffer to the next"
  (let ((buflist '("journal" "friday" "todo" "sinbad" "dodo" "schlag" "ado"))
        (expected '("todo" "dodo" "schlag" "ado")))
    (dolist (bufname buflist) (create-file-buffer bufname))
    (with-current-buffer "schlag"
      (do-mode))
    (dolist (exp expected)
      (next-dodo)
      (should (string= exp (buffer-name))))
    (dolist (bufname buflist)
      (kill-buffer bufname))))

;; ----------------------------------------------------------------------------
(ert-deftest test-2310-previous-dodo ()
  "previous-dodo: jump from one dodo buffer to the previous"
  (let ((buflist '("journal" "friday" "todo" "sinbad" "dodo" "schlag" "ado"))
        (expected '("ado" "dodo" "todo" "friday")))
    (dolist (bufname buflist) (create-file-buffer bufname))
    (with-current-buffer "friday"
      (do-mode))
    (dolist (exp expected)
      (previous-dodo)
      (should (string= exp (buffer-name))))
    (dolist (bufname buflist)
      (kill-buffer bufname))))

;; ============================================================================
;; tests for do-task-to-top

;; ----------------------------------------------------------------------------
(ert-deftest test-2400-task-to-top ()
  "do-task-to-top: In a zero-length file, this will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2405-task-to-top ()
  "do-task-to-top: In a file of whitespace, this will make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "w"))
      (goto-char (floor (point-max) 2))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2410-task-to-top ()
  "do-task-to-top: In a file with one task and no DONE line, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "-"))
      (goto-char (buffer-pos (task 1)))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2415-task-to-top ()
  "do-task-to-top: In a file with a DONE line and one task above, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- d"))
      (goto-char (buffer-pos (task 1)))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2420-task-to-top ()
  "do-task-to-top: In a file with a DONE line and one task below, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "d +"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2425-task-to-top ()
  "do-task-to-top: In a file with no DONE line and two tasks, this
will not move the top task (no changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- -"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2430-task-to-top ()
  "do-task-to-top: In a file with no DONE line and two tasks, this
will move the bottom task to top."
  (with-temp-buffer
    (let ((t1-pos) (t2-pos))
      (insert (make-data-s "- -"))
      (goto-char (buffer-pos (task 2)))
      (do-task-to-top)
      (setq t1-pos (buffer-pos (task 1)))
      (setq t2-pos (buffer-pos (task 2)))
      (should (< t2-pos t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2435-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks above, this
will not move the top task (no changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - d"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2440-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks above, this
will move the bottom task to top."
  (with-temp-buffer
    (let ((t1-pos) (t2-pos) (done-pos))
      (insert (make-data-s "- - d"))
      (goto-char (buffer-pos (task 2)))
      (do-task-to-top)
      (setq t1-pos (buffer-pos (task 1)))
      (setq t2-pos (buffer-pos (task 2)))
      (setq done-pos (do-done-position))
      (should (in-order-p t2-pos t1-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2445-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks below, this
will not move the top task (no changes)."
  (with-temp-buffer
    (let ((before) (t1-pos) (t2-pos) (done-pos))
      (insert (make-data-s "d + +"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-top)
      (setq t1-pos (buffer-pos (task 1)))
      (setq t2-pos (buffer-pos (task 2)))
      (setq done-pos (do-done-position))
      (should (string= before (buffer-string)))
      (should (in-order-p done-pos t1-pos t2-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2450-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks below, this
will move the bottom task to just below DONE line."
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos))
      (insert (make-data-s "d + +"))
      (goto-char (buffer-pos (task 2)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (setq t1-pos (buffer-pos (task 1)))
      (setq t2-pos (buffer-pos (task 2)))
      (should (in-order-p done-pos t2-pos t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2455-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will not move the top task above the DONE line (no
changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - - d + < x"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-top)
      (should (buffer-equal-p before (point-min)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2460-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the middle task above the
DONE line to the beginning of the file."
  (with-temp-buffer
    (let ((tpos-l) (done-pos))
      (insert (make-data-s "- - - d + < x"))
      (goto-char (buffer-pos (task 2) 3))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (dolist (vdx (list 2 1 3 4 5 6))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2465-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the bottom task above the
DONE line to the top of the file."
  (with-temp-buffer
    (let ((done-pos) (tpos-l))
      (insert (make-data-s "- - - d < x +"))
      (goto-char (buffer-pos (task 3) +5))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (dolist (vdx '(3 1 2 4 5 6))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2470-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will not move the top task below the
DONE line (no changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - - d < x +"))
      (goto-char (buffer-pos (task 4) 5))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2475-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the middle task below the
DONE line to just below the DONE line."
  (with-temp-buffer
    (let ((tpos-l ()) (done-pos))
      (insert (make-data-s "- - - d x + <"))
      (goto-char (buffer-pos (task 5)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      ;; This list of indexes appears out of order because we expect
      ;; task 5 to have moved above task 4.
      (dolist (vdx '(1 2 3 5 4 6))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2480-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the bottom task below the
DONE line to just below the DONE line."
  (with-temp-buffer
    (let ((tpos-l ()) (done-pos))
      (insert (make-data-s "- - - d + < x"))
      (goto-char (buffer-pos (task 6)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (dolist (vdx (list 1 2 3 6 4 5))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ============================================================================
;; tests for do-task-to-end

;; ----------------------------------------------------------------------------
(ert-deftest test-2500-task-to-end ()
  "do-task-to-end: In a zero-length file, do-task-to-end should
make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)                  ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2505-task-to-end ()
  "do-task-to-end: In a file of whitespace, do-task-to-end should
make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "w"))
      (goto-char (floor (point-max) 2))
      (setq before (buffer-string))
      (do-task-to-end)                  ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2510-task-to-end ()
  "do-task-to-end: In a file with one task and no DONE line, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "-"))
      (goto-char (buffer-pos (task 1)))
      (setq before (buffer-string))
      (do-task-to-end)                  ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2515-task-to-end ()
  "do-task-to-end: In a file with a DONE line and one task above, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- d"))
      (goto-char (buffer-pos (task 1)))
      (setq before (buffer-string))
      (do-task-to-end)                  ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2520-task-to-end ()
  "do-task-to-end: In a file with a DONE line and one task below, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "d -"))
      (goto-char (buffer-pos (task 1)))
      (setq before (buffer-string))
      (do-task-to-end)                  ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2525-task-to-end ()
  "do-task-to-end: In a file with no DONE line and two tasks, this
will not move the bottom task (no changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- -"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 2)))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2530-task-to-end ()
  "do-task-to-end: In a file with no DONE line and two tasks, this
will move the top task to the end."
  (with-temp-buffer
    (let ((t1-pos) (t2-pos))
      (insert (make-data-s "- -"))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-end)
      (setq t1-pos (buffer-pos (task 1)))
      (setq t2-pos (buffer-pos (task 2)))
      (should (in-order-p t2-pos t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2535-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks above, this
will not move the bottom task (no changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - d"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 2)))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2540-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks above, this
will move the top task to just before the DONE line."
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos))
      (insert (make-data-s "- - d"))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-end)
      (setq t1-pos (buffer-pos (task 1)))
      (setq t2-pos (buffer-pos (task 2)))
      (setq done-pos (do-done-position))
      (should (in-order-p t2-pos t1-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2545-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks below, this
will not move the bottom task (no changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "d + +"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 2)))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2550-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks below, this
will move the top task to the end of the file."
  (with-temp-buffer
    (let ((done-pos) (t1-pos) (t2-pos))
      (insert (make-data-s "d - -"))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (setq t1-pos (buffer-pos (task 1)))
      (setq t2-pos (buffer-pos (task 2)))
      (should (in-order-p done-pos t2-pos t1-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2555-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will not move the bottom task above the DONE line (no
changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - - d + < x"))
      (goto-char (buffer-pos (task 3)))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2560-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the middle task above the
DONE line to just before the DONE line."
  (with-temp-buffer
    (let ((done-pos) (tpos-l ()))
      (insert (make-data-s "- - - d + < x"))
      (goto-char (buffer-pos (task 2)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (dolist (vdx '(1 3 2 4 5 6))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2565-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the top task above the
DONE line to just before the DONE line."
  (with-temp-buffer
    (let ((done-pos) (tpos-l ()))
      (insert (make-data-s "- - - d x + <"))
      (goto-char (buffer-pos (task 1)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (dolist (vdx '(2 3 1 4 5 6))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2570-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will not move the bottom task below the
DONE line (no changes)."
  (with-temp-buffer
    (let ((before))
      (insert (make-data-s "- - - d < x +"))
      (setq before (buffer-string))
      (goto-char (buffer-pos (task 6)))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2575-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the middle task below the
DONE line to the end of the file."
  (with-temp-buffer
    (let ((done-pos) (tpos-l ()))
      (insert (make-data-s "- - - d x + <"))
      (goto-char (buffer-pos (task 5)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (dolist (vdx '(1 2 3 4 6 5))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2580-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the top task below the
DONE line to the end of the file."
  (with-temp-buffer
    (let ((done-pos) (tpos-l ()))
      (insert (make-data-s "- - - d < x +"))
      (goto-char (buffer-pos (task 4)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (dolist (vdx '(1 2 3 5 6 4))
        (setq tpos-l (cl-list* (buffer-pos (task vdx)) tpos-l)))
      (setq tpos-l (reverse tpos-l))
      (should (apply #'in-order-p tpos-l))
      (should (in-order-p (nth 2 tpos-l) done-pos (nth 3 tpos-l)))
      )))

;; ----------------------------------------------------------------------------
;; Copy this to *scratch* and eval-buffer (esc-b) to run the tests
;; interactively
;;
;; (reload-do-mode)
;; (ert-run-tests-interactively "t")
