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
(setq g-1st-task "1st task")
(setq g-2nd-sample "2nd sample")
(setq g-2nd-task "2nd task")
(setq g-3rd-sample "3rd sample")
(setq g-abandoned "\n\n x abandoned task number 2\n")
; (setq g-abandoned-1st "---\n\n x 1st")
(setq g-abc "abc")
(setq g-alphabet "abcdefghijklmnopqrstuvwxyz")
(setq g-also-sp "also ")
(setq g-artemis "artemis")
(setq g-both-do "both.do")
(setq g-buf-no-done "      \n\n - first task\n - second task\n")
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
(setq g-dash-1st " - 1st")
(setq g-dash-2nd " - 2nd")
(setq g-dash-3rd " - 3rd")
(setq g-dash-1st-sample "- 1st sample")
(setq g-dash-2nd-sample " - 2nd sample")
(setq g-dash-3rd-sample " - 3rd sample")
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
(setq g-less-2nd-sample " < 2nd sample")
(setq g-less-3rd-sample " < 3rd sample")
(setq g-mnop "mnop")
(setq g-n-e-sp-2-new "ne 2\n")
(setq g-new-new "\n\n")
(setq g-new-new-sp "\n\n ")
(setq g-new3-dash "\n\n\n -")
(setq g-new-sp-dash "\n -")
(setq g-new-task-rgx " - \\[[.0-9]\\{9\\}\\] ")
(setq g-no-active-tasks "no active tasks found")
(setq g-no-tasks "no tasks in file")
(setq g-plus-1st " \\+ 1st")
(setq g-plus-1st-task "\n\n + 1st task\n\n")
(setq g-plus-1st-sample " \\+ 1st sample")
(setq g-plus-2nd " \\+ 2nd")
(setq g-plus-3rd-sample " \\+ 3rd sample")
(setq g-plus-also "+ also")
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
(setq g-t-a-s "tas")
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
(defun get-message-max ()
 "Get the maximum size of the *Messages* buffer so we can track
subsequent messages."
  (with-current-buffer g-msgbuf-name
    (point-max)))

;; ----------------------------------------------------------------------------
(defun in-messages-p (after needle)
  "Return the index of NEEDLE if it is in buffer *Messages*
following AFTER. If NEEDLE is not found, return nil.\n"
  (with-current-buffer g-msgbuf-name
    (string-match needle (buffer-substring after (point-max)))))

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
            (setq rval (concat rval "\n\n" g-done-line))
          (if (string= "n" item)
              (setq rval (concat rval "\n"))))))
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


;; ============================================================================
;; tests for do-add-done-iff

;; ----------------------------------------------------------------------------
(ert-deftest test-1000-add-done-iff-empty ()
  "The payload function should add a done line if there isn't one
in the buffer already. In this test, the buffer is empty."
  (with-temp-buffer
    (do-add-done-iff)
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
      (insert g-buf-no-done)
      (goto-char (point-min))
      (do-add-done-iff)
      (setq done-pos (do-done-position))
      (goto-char (point-max))
      (setq ltask-pos (re-search-backward do-mode-rgx-task))
      (should (not (equal nil done-pos)))
      (should (not (equal nil ltask-pos)))
      (should (< ltask-pos done-pos)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1020-add-done-iff-present ()
  "Verify that do-add-done-iff doesn't add a done line if one is
already present"
  (with-temp-buffer
    (insert g-three-new do-mode-done-line g-six-new)
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))
    (do-add-done-iff)
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))
    (should (equal nil (re-search-forward do-mode-rgx-done nil 't)))))

;; ============================================================================
;; tests for bytes-at

;; ----------------------------------------------------------------------------
(ert-deftest test-1090-bytes-at-zero-min ()
  "bytes-at: zero-length buffer point min"
  (with-temp-buffer
    (should (string= g-empty (bytes-at (point-min) 3)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1091-bytes-at-zero-mid ()
  "bytes-at: zero-length buffer point middle"
  (with-temp-buffer
    (should (string= g-empty (bytes-at (point) 3)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1092-bytes-at-zero-max ()
  "bytes-at: zero-length buffer point max"
  (with-temp-buffer
    (should (string= g-empty (bytes-at (point-max) 3)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1093-bytes-at-short-min ()
  "bytes-at: short buffer point min"
  (with-temp-buffer
    (let ((source))
      (setq source (substring g-alphabet 0 2))
      (insert source)
      (should (string= source (bytes-at (point-min) 10)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1094-bytes-at-short-mid ()
  "bytes-at: short buffer point middle"
  (with-temp-buffer
    (let ((source))
      (setq source (substring g-alphabet 0 2))
      (insert source)
      (should (string= source (bytes-at 2 10)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1095-bytes-at-short-max ()
  "bytes-at: short buffer point max"
  (with-temp-buffer
    (let ((source))
      (setq source (substring g-alphabet 0 2))
      (insert source)
      (should (string= source (bytes-at (point-max) 10)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1096-bytes-at-long-min ()
  "bytes-at: long buffer point min"
  (with-temp-buffer
    (insert g-alphabet)
    (should (string= g-abc (bytes-at (point-min) 3)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1097-bytes-at-long-mid ()
  "bytes-at: long buffer point middle"
  (with-temp-buffer
    (insert g-alphabet)
    (should (string= g-mnop (bytes-at 13 4)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1098-bytes-at-long-max ()
  "bytes-at: long buffer point max"
  (with-temp-buffer
    (insert g-alphabet)
    (should (string= g-vwxyz (bytes-at (point-max) 5)))
    ))

;; ============================================================================
;; tests for do-done-position

;; ----------------------------------------------------------------------------
(ert-deftest test-1100-do-done-position-no-done ()
  "test do-done-position when DONE line is missing"
  (with-temp-buffer
    (insert g-six-new)
    (should (equal nil (do-done-position)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1110-do-done-position-with-done ()
  "test do-done-position when DONE line is present"
  (with-temp-buffer
    (insert g-three-sp g-three-new g-done-line g-three-new)
    (should (= 7 (do-done-position)))))


;; ============================================================================
;; tests for do-goto-next-task
;;
;; Note that do-goto-next-task searches right through the DONE line if
;; it's there. It simply moves to the next task entry in the file,
;; even if it's completed or marked in some other way.
;;
;; (no DONE line)
;;   1210: point is before first task in file -> first task
;;   1215: point is on newline before first task -> first task
;;   1220: point is at beginning of first task mark -> second task
;;   1230: point is at hyphen of first task mark -> second task
;;   1240: point is at second space of first task mark -> second task
;;   1250: point is way down into first task -> second task
;;   1255: point is at \n before second task -> second task
;;   1260: point is at beginning of second task -> point-max
;;   1270: point is one pos into second task -> point-max
;;   1280: point is two bytes into second task -> point-max
;;   1290: point is at end of file
;;
;; (*with* DONE line)
;;   1310: beginning of file -> first task
;;   1320: \n before first task -> first task
;;   1330: at first task -> second task
;;   1340: one byte into first task -> second task
;;   1350: two bytes into first task -> second task
;;   1360: at "k o" -> second task
;;   1370: last \n in task 1 -> second task
;;   1380: at task two -> task three
;;   1390: at 'tas' in task two -> task three
;;   1392: at ' \n ' in task two -> task three
;;   1394: at task 3 -> first finished
;;   1396: at '\n\n\n' in task 3 -> first finished
;;   1398: at '+ f' in finished -> second finished

;; ----------------------------------------------------------------------------
(ert-deftest test-1210-next-no-done ()
  "goto-next: no DONE line, two tasks, point well before first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 2)
    (should (= 9 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1215-next-no-done ()
  "goto-next: no DONE, two tasks, point just before first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 7)
    (should (string= g-new-new-sp (bytes-at (point) 3)))
    (should (= 9 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1220-next-no-done ()
  "goto-next: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 9)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1230-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 10)
    (should (string= g-dash-sp-f (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1240-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 11)
    (should (string= g-sp-f-i (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1250-next-no-done ()
  "goto-next: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 16)
    (should (string= g-t-sp-t (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1255-next-no-done ()
  "goto-next: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 22)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1260-next-no-done ()
  "goto-next: no DONE, two tasks, point at second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 23)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1270-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 24)
    (should (string= g-dash-sp-s (bytes-at (point) 3)))
    (should (= 24 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1280-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 25)
    (should (string= g-sp-s-e (bytes-at (point) 3)))
    (should (= (point) (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1290-next-no-done ()
  "goto-next: no DONE, two tasks, point near end of second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char (point-max))
    (should (string= g-s-k-new (bytes-at (- (point-max) 3) 3)))
    (should (= (point-max) (do-goto-next-task)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1310-next-w-done ()
  "goto-next: with DONE, four tasks, point at start of buffer"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 1)
    (should (string= g-three-sp (bytes-at (point) 3)))
    (should (= 8 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1320-next-w-done ()
  "goto-next: with DONE, four tasks, point just before 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 7)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 8 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1330-next-w-done ()
  "goto-next: with DONE, four tasks, point at 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 8)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1340-next-w-done ()
  "goto-next: with DONE, four tasks, point one byte into 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 9)
    (should (string= g-dash-sp-t (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1350-next-w-done ()
  "goto-next: with DONE, four tasks, point two bytes into 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 10)
    (should (string= g-sp-t-a (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1360-next-w-done ()
  "goto-next:  with DONE, four tasks, point in middle of 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 14)
    (should (string= g-k-sp-o (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1370-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 2nd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 20)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1380-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 2nd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 21)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1390-next-w-done ()
  "goto-next:  with DONE, four tasks, point a couple bytes into 2nd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 24)
    (should (string= g-t-a-s (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1392-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 3rd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 33)
    (should (string= g-sp-new-new (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1394-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 3rd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 35)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))
    (should (string= g-sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1396-next-w-done ()
  "goto-next:  with DONE, four tasks, point at end of 3rd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 48)
    (should (string= g-e-new-new (bytes-at (point) 3)))
    (should (= 106 (do-goto-next-task)))
    (should (string= g-sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1398-next-w-done ()
  "goto-next:  with DONE, four tasks, point in 4th (completed) task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 107)
    (should (string= g-plus-sp-f (bytes-at (point) 3)))
    (should (= 121 (do-goto-next-task)))
    (should (string= g-sp-plus-sp (bytes-at 106 3)))
    (should (string= g-sp-plus-sp (bytes-at 121 3)))))

;; ============================================================================
;; tests for do-goto-prev-task
;;
;; We'll start at the bottom of the buffer and work backwards across
;; the DONE line if it's there

;; ----------------------------------------------------------------------------
(ert-deftest test-1400-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char (point-max))
      (should (string= g-n-e-sp-2-new (bytes-at (point) 5)))
      (setq result (do-goto-prev-task))
      (should (= 121 result))
      (should (string= g-sp-plus-also (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1405-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 124)
      (should (string= g-also-sp (bytes-at (point) 5)))
      (setq result (do-goto-prev-task))
      (should (= 121 result))
      (should (string= g-sp-plus-also (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1410-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 123)
      (should (string= g-sp-also-sp (bytes-at (point) 6)))
      (setq result (do-goto-prev-task))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1420-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task by 1 byte"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 122)
      (should (string= g-plus-also (bytes-at (point) 6)))
      (setq result (do-goto-prev-task))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1430-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at last task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 121)
      (should (= 121 (point)))
      (should (string= g-sp-plus-also (bytes-at (point) 7)))
      (setq result (do-goto-prev-task))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1440-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 110)
      (should (string= g-inish (bytes-at (point) 5)))
      (setq result (do-goto-prev-task))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1442-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point early in penultimate task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 109)
      (should (string= g-finis (bytes-at (point) 5)))
      (setq result (do-goto-prev-task))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1444-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task mark"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 108)
      (should (string= g-sp-fini (bytes-at (point) 5)))
      (setq result (do-goto-prev-task))
      (should (= 36 result))
      (should (string= g-dash-task-thr (bytes-at result 11))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1450-prev-w-done ()
  "prev task: with DONE, 5 tasks, point before 1st task, land on 1st"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 5)
      (should (string= g-new3-dash (bytes-at (point) 5)))
      (setq result (do-goto-prev-task))
      (should (= 8 result))
      (should (string= g-dash-tas (bytes-at result 6))))))

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
    (let ((ntask-pos) (otask-pos))
      (insert g-buf-samples1)
      (goto-char (point-min))
      (do-new-task)                     ; payload
      (goto-char (point-min))
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq otask-pos
                    (string-match g-dash-singsamp (buffer-string))))
      (should (< ntask-pos otask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1610-new-003 ()
  "new entry: no DONE line, one task, after 1st"
  (with-temp-buffer
    (let ((ntask-pos) (otask-pos))
      (insert g-buf-samples1)
      (goto-char (string-match g-task (buffer-string)))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq otask-pos (string-match g-dash-singsamp (buffer-string))))
      (should (< otask-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1615-new-004 ()
  "new entry: no DONE line, two tasks, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert g-buf-samples2)
      (goto-char (point-min))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match g-dash-1st (buffer-string))))
      (should (setq second-pos (string-match g-dash-2nd (buffer-string))))
      (should (< ntask-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1620-new-005 ()
  "new entry: no DONE line, two tasks, before 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert g-buf-samples2)
      (goto-char (- (string-match g-dash-2nd-sample (buffer-string)) 5))
      (do-new-task)                     ; payload
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match g-dash-1st (buffer-string))))
      (should (setq second-pos (string-match g-dash-2nd (buffer-string))))
      (should (< first-pos ntask-pos))
      (should (< ntask-pos second-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1625-new-006 ()
  "new entry: no DONE line, two tasks, after 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert g-buf-samples2)
      (goto-char (- (point-max) 1))
      (do-new-task)
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match g-dash-1st (buffer-string))))
      (should (setq second-pos (string-match g-dash-2nd (buffer-string))))
      (should (< first-pos ntask-pos))
      (should (< second-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1630-new-007 ()
  "new entry: no DONE line, three tasks, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3)
      (goto-char 2)
      (do-new-task)
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match g-dash-1st (buffer-string))))
      (should (setq second-pos (string-match g-dash-2nd (buffer-string))))
      (should (setq third-pos (string-match g-dash-3rd (buffer-string))))
      (should (< ntask-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1635-new-008 ()
  "new entry: no DONE line, three tasks, before 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3)
      (goto-char 3)
      (end-of-line)
      (do-new-task)
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match g-dash-1st (buffer-string))))
      (should (setq second-pos (string-match g-dash-2nd (buffer-string))))
      (should (setq third-pos (string-match g-dash-3rd (buffer-string))))
      (should (< first-pos ntask-pos))
      (should (< ntask-pos second-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1640-new-009 ()
  "new entry: no DONE line, three tasks, before 3rd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3)
      (goto-char (+ (string-match g-2nd-sample (buffer-string)) 10))
      (do-new-task)
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match g-dash-1st (buffer-string))))
      (should (setq second-pos (string-match g-dash-2nd (buffer-string))))
      (should (setq third-pos (string-match g-dash-3rd (buffer-string))))
      (should (< second-pos ntask-pos))
      (should (< ntask-pos third-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1645-new-010 ()
  "new entry: no DONE line, three tasks, after 3rd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3)
      (goto-char (+ (string-match g-3rd-sample (buffer-string)) 10))
      (do-new-task)
      (should (setq ntask-pos (string-match g-new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match g-dash-1st (buffer-string))))
      (should (setq second-pos (string-match g-dash-2nd (buffer-string))))
      (should (setq third-pos (string-match g-dash-3rd (buffer-string))))
      (should (< third-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1650-new-011 ()
  "new entry: DONE line present, no tasks"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert g-done-line)
      (goto-char (point-min))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match g-new-task-rgx (buffer-string)))
      (should (< ntask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1655-new-011 ()
  "new entry: DONE line present, no tasks"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert g-done-line)
      (goto-char (point-max))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match g-new-task-rgx (buffer-string)))
      (should (< ntask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1660-new-012 ()
  "new entry: DONE line present, one task, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert g-buf-samples1 g-done-line)
      (goto-char (point-min))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match g-new-task-rgx (buffer-string)))
      (setq first-pos (string-match g-dash-singsamp (buffer-string)))
      (should (< ntask-pos done-pos))
      (should (< ntask-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1665-new-012 ()
  "new entry: DONE line present, one task, after 1st"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos) (first-pos))
      (insert g-buf-samples1 g-done-line g-new-new)
      (goto-char (string-match g-task (buffer-string)))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match g-new-task-rgx (buffer-string)))
      (setq first-pos (string-match g-dash-singsamp (buffer-string)))
      (should (< ntask-pos done-pos))
      (should (< first-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1670-new-012 ()
  "new entry: DONE line present, one task, after done line"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert g-buf-samples1 g-done-line g-new-new)
      (goto-char (point-max))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match g-new-task-rgx (buffer-string)))
      (setq first-pos (string-match g-dash-singsamp (buffer-string)))
      (should (< ntask-pos done-pos))
      (should (< first-pos ntask-pos))
      )))

;; ============================================================================
;; tests for do-next-task-mark
;;

;; ----------------------------------------------------------------------------
(ert-deftest test-1700-ntm-no-done ()
  "next-task: no DONE line, two tasks, point well before first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 2)
    (should (= 9 (do-next-task-mark)))
    (should (string= g-dash-first (bytes-at 9 8)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1703-ntm-no-done ()
  "next-task: no DONE, two tasks, point just before first task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-no-done)
      (goto-char 7)
      (should (string= g-new-new-sp (bytes-at (point) 3)))
      (setq result (do-next-task-mark))
      (should (= 9 result))
      (should (string= g-dash-first (bytes-at result 8))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1706-ntm-no-done ()
  "next-task: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 9)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1709-ntm-no-done ()
  "next-task: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 10)
    (should (string= g-dash-sp-f (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1712-ntm-no-done ()
  "next-task: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 11)
    (should (string= g-sp-f-i (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1715-ntm-no-done ()
  "next-task: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 16)
    (should (string= g-t-sp-t (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1718-ntm-no-done ()
  "next-task: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 22)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1721-ntm-no-done ()
  "next-task: no DONE, two tasks, point at second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 23)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1724-ntm-no-done ()
  "next-task: no DONE, two tasks, point just inside second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 24)
    (should (string= g-dash-sp-s (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1727-ntm-no-done ()
  "next-task: no DONE, two tasks, point two bytes into second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char 25)
    (should (string= g-sp-s-e (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1730-ntm-no-done ()
  "next-task: no DONE, two tasks, point near end of second task"
  (with-temp-buffer
    (insert g-buf-no-done)
    (goto-char (point-max))
    (should (string= g-s-k-new (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1733-ntm-w-done ()
  "next-task: with DONE, four tasks, point at start of buffer"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 1)
    (should (string= g-three-sp (bytes-at (point) 3)))
    (should (= 8 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1736-ntm-w-done ()
  "next-task: with DONE, four tasks, point just before 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 7)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 8 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1739-ntm-w-done ()
  "next-task: with DONE, four tasks, point at 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 8)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1742-ntm-w-done ()
  "next-task: with DONE, four tasks, point one byte into 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 9)
    (should (string= g-dash-sp-t (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1745-ntm-w-done ()
  "next-task: with DONE, four tasks, point two bytes into 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 10)
    (should (string= g-sp-t-a (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1748-ntm-w-done ()
  "next-task:  with DONE, four tasks, point in middle of 1st task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 14)
    (should (string= g-k-sp-o (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1751-ntm-w-done ()
  "next-task:  with DONE, four tasks, point just before 2nd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 20)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1754-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at 2nd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 21)
    (should (string= g-sp-dash-sp (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1757-ntm-w-done ()
  "next-task:  with DONE, four tasks, point a couple bytes into 2nd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 24)
    (should (string= g-t-a-s (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1760-ntm-w-done ()
  "next-task:  with DONE, four tasks, point just before 3rd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 33)
    (should (string= g-sp-new-new (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1763-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at 3rd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 35)
    (should (string= g-new-sp-dash (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))
    (should (string= g-sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1766-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at end of 3rd task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 48)
    (should (string= g-e-new-new (bytes-at (point) 3)))
    (should (= 106 (do-next-task-mark)))
    (should (string= g-sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1769-ntm-w-done ()
  "next-task:  with DONE, four tasks, point in 4th (completed) task"
  (with-temp-buffer
    (insert g-buf-w-done)
    (goto-char 107)
    (should (string= g-plus-sp-f (bytes-at (point) 3)))
    (should (= 121 (do-next-task-mark)))
    (should (string= g-sp-plus-sp (bytes-at 106 3)))
    (should (string= g-sp-plus-sp (bytes-at 121 3)))))

;; ============================================================================
;; tests for do-prev-task-mark
;;

;; ----------------------------------------------------------------------------
(ert-deftest test-1800-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char (point-max))
      (should (string= g-n-e-sp-2-new (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 121 result))
      (should (string= g-sp-plus-also (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1805-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 124)
      (should (string= g-also-sp (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 121 result))
      (should (string= g-sp-plus-also (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1810-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in last task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 123)
      (should (string= g-sp-also-sp (bytes-at (point) 6)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1815-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in last task by 1 byte"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 122)
      (should (string= g-plus-also (bytes-at (point) 6)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1820-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at last task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 121)
      (should (= 121 (point)))
      (should (string= g-sp-plus-also (bytes-at (point) 7)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1825-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in penultimate task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 110)
      (should (string= g-inish (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1830-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point early in penultimate task"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 109)
      (should (string= g-finis (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= g-plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1835-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in penultimate task mark"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 108)
      (should (string= g-sp-fini (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 36 result))
      (should (string= g-dash-task-thr (bytes-at result 11))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1840-ptm-w-done ()
  "prev task: with DONE, 5 tasks, point before 1st task, land on 1st"
  (with-temp-buffer
    (let ((result))
      (insert g-buf-w-done)
      (goto-char 5)
      (should (string= g-new3-dash (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 8 result))
      (should (string= g-dash-tas (bytes-at result 6))))))

;; ============================================================================
;; tests for do-[pxo]done

;; ----------------------------------------------------------------------------
(ert-deftest test-1900-pdone-empty ()
  "pdone: empty file doesn't change"
  (with-temp-buffer
    (let ((msg-max (get-message-max)))
      (do-pdone 't)                     ; payload
      (should (in-messages-p msg-max g-file-too-small))
      (should (= (point-max) 1)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1905-xdone-whitespace ()
  "xdone: file of whitespace doesn't change"
  (with-temp-buffer
    (let ((msg-max (get-message-max))
          (before)
          (pre-point))
      (insert g-whitespace)
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
      (insert do-mode-done-line)
      (insert g-completed)
      (insert g-abandoned)
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
      (insert g-sample-task)
      (goto-char (string-match g-sample (buffer-string)))
      (do-pdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (goto-char (point-max))
      (setq task-pos (re-search-backward do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-plus-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1920-xdone-ndl-two-1st ()
  "two task, no DONE: DONE line added, first task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert g-buf-samples2)
      (goto-char (string-match g-sp-first (buffer-string)))
      (do-xdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-x-first)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1925-odone-ndl-two-2nd ()
  "two task, no DONE: DONE line added, second task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert g-buf-samples2)
      (goto-char (string-match g-sp-second (buffer-string)))
      (do-odone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-less-2nd-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1930-pdone-ndl-three-1st ()
  "three tasks, no DONE: DONE line added, first task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert g-buf-samples3)
      (goto-char (string-match g-dash-1st-sample (buffer-string)))
      (do-pdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-plus-1st-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1935-xdone-ndl-three-2nd ()
  "three tasks, no DONE: DONE line added, middle task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert g-buf-samples3)
      (goto-char (string-match g-2nd-sample (buffer-string)))
      (do-xdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-x-second-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1940-odone-ndl-three-3rd ()
  "three tasks, no DONE: DONE line added, last task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert g-buf-samples3)
      (goto-char (string-match g-3rd-sample (buffer-string)))
      (do-odone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-less-3rd-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1945-pdone-wdl-one ()
  "one task, with DONE: task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert g-buf-samples1 g-done-line)
      (goto-char (string-match g-sample (buffer-string)))
      (do-pdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-plus-single)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1950-xdone-wdl-two-1st ()
  "two tasks, with DONE: first task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert g-buf-samples2 g-done-line)
      (goto-char (string-match g-sample (buffer-string)))
      (do-xdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-x-first)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1955-odone-wdl-two-2nd ()
  "two tasks, with DONE: second task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert g-buf-samples2 g-done-line)
      (goto-char (string-match g-2nd-sample (buffer-string)))
      (do-odone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-less-2nd-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1960-pdone-wdl-three-1st ()
  "three tasks, with DONE: 1st task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert g-buf-samples3 g-done-line)
      (goto-char (string-match g-sp-first (buffer-string)))
      (do-pdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-plus-1st-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1965-xdone-wdl-three-2nd ()
  "three tasks, with DONE: 2nd task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert g-buf-samples3 g-done-line)
      (goto-char (string-match g-2nd-sample (buffer-string)))
      (do-xdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-x-second-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1970-odone-wdl-three-3rd ()
  "three tasks, with DONE: 3rd task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert g-buf-samples3 g-done-line)
      (goto-char (string-match g-dash-3rd-sample (buffer-string)))
      (forward-char)
      (do-odone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position g-less-3rd-sample)))
      )))

;; ----------------------------------------------------------------------------
;;  three tasks, with DONE: 1st, 2nd move below without concatenation
(ert-deftest test-1975-odone-wdl-three-3rd ()
  "three tasks, with DONE: 3rd task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert g-buf-samples3 g-done-line)
      (goto-char (string-match g-dash-2nd-sample (buffer-string)))
      (forward-char)
      (do-odone 't)                      ; payload
      (do-pdone 't)                      ; payload
      (setq done-pos (do-done-position))
      (setq ltask-pos (last-position do-mode-rgx-task))
      (setq ptask-pos (last-position do-mode-rgx-task ltask-pos))
      (should (< ptask-pos ltask-pos))
      (should (< done-pos ptask-pos))
      (should (= ltask-pos (last-position g-less-2nd-sample)))
      (should (= ptask-pos (last-position g-plus-3rd-sample ltask-pos)))
      (should (equal nil (string-match g-dash-3rd-sample (buffer-string))))
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
    (should (not (do-buffer-p bufname)))
    (kill-buffer bufname)))

;; ----------------------------------------------------------------------------
(ert-deftest test-2015-dobufp-other-yes-mode ()
  "other buffer *is* a do-buffer by mode"
  (let ((bufname g-hercules))
    (create-file-buffer bufname)
    (with-current-buffer bufname
      (do-mode))
    (should (do-buffer-p bufname))
    (kill-buffer bufname)))

;; ----------------------------------------------------------------------------
(ert-deftest test-2020-dobufp-other-yes-name ()
  "other buffer is a do-buffer by name"
  (let ((bufname g-xyz-do))
    (create-file-buffer bufname)
    (should (do-buffer-p bufname))
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
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2105-do-task-up ()
  "do-task-up: whitespace -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert "                    ")
      (setq before (buffer-string))
      (goto-char 8)
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2110-do-task-up ()
  "do-task-up: one task, no DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert g-plus-1st-task)
      (setq before (buffer-string))
      (goto-char (string-match g-first (buffer-string)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2115-do-task-up ()
  "do-task-up: one task, above DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert g-plus-1st-task g-done-line)
      (setq before (buffer-string))
      (goto-char (string-match g-first (buffer-string)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
  )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2120-do-task-up ()
  "do-task-up: one task, above DONE -- moving DONE doesn't do anything"
  (with-temp-buffer
    (let ((before))
      (insert g-plus-1st-task g-done-line)
      (setq before (buffer-string))
      (goto-char (string-match g-done (buffer-string)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2125-do-task-up ()
  "do-task-up: one task, below DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert g-done-line g-plus-1st-task)
      (setq before (buffer-string))
      (goto-char (string-match g-first (buffer-string)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2130-do-task-up ()
  "do-task-up: two tasks, no DONE -- top doesn't move"
  (with-temp-buffer
    (let ((before))
      (insert g-two-tasks)
      (setq before (buffer-string))
      (goto-char (string-match g-first (buffer-string)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
  )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2135-do-task-up ()
  "do-task-up: two tasks, no DONE -- bottom moves past top"
  (with-temp-buffer
    (let ((before))
      (insert g-two-tasks)
      (goto-char (string-match g-second (buffer-string)))
      (do-task-up)                      ; payload
      (setq second-pos (string-match g-dash-2nd (buffer-string)))
      (setq first-pos (string-match g-dash-1st (buffer-string)))
      (should (equal nil (do-done-position)))
      (should (< second-pos first-pos))
      (should (= (point) (+ 1 second-pos)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2140-do-task-up ()
  "do-task-up: two tasks, above DONE -- bottom moves past top"
  (with-temp-buffer
    (let ((before) (first-pos) (second-pos) (done-pos))
      (insert g-two-tasks g-done-line)
      (setq before (buffer-string))
      (goto-char (string-match g-second (buffer-string)))
      (do-task-up)                      ; payload
      (setq second-pos (string-match g-dash-2nd (buffer-string)))
      (setq first-pos (string-match g-dash-1st (buffer-string)))
      (setq done-pos (do-done-position))
      (should (< second-pos first-pos))
      (should (< first-pos done-pos))
      (should (= (point) (+ 1 second-pos)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2145-do-task-up ()
  "do-task-up: two tasks, above DONE -- DONE won't move past bottom"
  (with-temp-buffer
    (let ((before))
      (insert g-two-tasks g-done-line)
      (setq before (buffer-string))
      (goto-char (+ 5 (do-done-position)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2150-do-task-up ()
  "do-task-up: two tasks, below DONE -- bottom moves past top"
  (with-temp-buffer
    (let ((before) (first-pos) (second-pos))
      (insert g-done-line g-plus-two-tasks)
      (setq before (buffer-string))
      (goto-char (string-match-end g-second (buffer-string)))
      (do-task-up)                      ; payload
      (setq first-pos (string-match g-plus-1st (buffer-string)))
      (setq second-pos (string-match g-plus-2nd (buffer-string)))
      (should (< second-pos first-pos))
      (should (< (do-done-position) second-pos))
      (should (= (point) (+ 1 second-pos)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2155-do-task-up ()
  "do-task-up: two tasks, below DONE -- top won't move past DONE"
  (with-temp-buffer
    (let ((before))
      (insert g-done-line g-plus-two-tasks)
      (setq before (buffer-string))
      (goto-char (string-match-end g-first (buffer-string)))
      (do-task-up)                      ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2160-do-task-up ()
  "do-task-up: three tasks, DONE after 2 -- upping 2nd should not
take DONE with it"
  (with-temp-buffer
    (let ((first-pos) (second-pos) (done-pos) (post-pos))
      (insert "\n\n - 1st task\n\n - 2nd task\n\n" g-done-line
              "\n\n + completed task")
      (goto-char (string-match-end g-second (buffer-string)))
      (do-task-up)                      ; payload
      (setq first-pos (string-match " - 1st task" (buffer-string)))
      (setq second-pos (string-match " - 2nd task" (buffer-string)))
      (setq done-pos (string-match "--- DONE ---" (buffer-string)))
      (setq post-pos (string-match " \\+ completed" (buffer-string)))
      (should (< second-pos first-pos))
      (should (< first-pos done-pos))
      (should (< done-pos post-pos))
      (should (= (point) (+ 1 second-pos)))
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
      (insert "                    ")
      (setq before (buffer-string))
      (goto-char 8)
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2210-do-task-down ()
  "do-task-down: one task, no DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert "\n\n + 1st task\n\n")
      (setq before (buffer-string))
      (goto-char (string-match "1st" (buffer-string)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2215-do-task-down ()
  "do-task-down: one task, above DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert "\n\n + 1st task\n\n" g-done-line)
      (setq before (buffer-string))
      (goto-char (string-match "1st" (buffer-string)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2220-do-task-down ()
  "do-task-down: one task, above DONE -- moving DONE doesn't do anything"
  (with-temp-buffer
    (let ((before))
      (insert "\n\n + 1st task\n\n" g-done-line)
      (setq before (buffer-string))
      (goto-char (string-match "DONE" (buffer-string)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2225-do-task-down ()
  "do-task-down: one task, below DONE -- do nothing"
  (with-temp-buffer
    (let ((before))
      (insert g-done-line "\n\n + 1st task\n\n")
      (setq before (buffer-string))
      (goto-char (string-match "1st" (buffer-string)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2230-do-task-down ()
  "do-task-down: two tasks, no DONE -- bottom doesn't move"
  (with-temp-buffer
    (let ((before))
      (insert "\n\n - 1st task\n\n - 2nd task\n\n")
      (setq before (buffer-string))
      (goto-char (string-match "2nd" (buffer-string)))
      (do-task-down)                    ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2235-do-task-down ()
  "do-task-down: two tasks, no DONE -- top moves past bottom"
  (with-temp-buffer
    (let ((before) (second-pos) (first-pos))
      (insert "\n\n - 1st task\n\n - 2nd task\n\n")
      (goto-char (string-match "1st" (buffer-string)))
      (do-task-down)                    ; payload
      (setq second-pos (string-match " - 2nd" (buffer-string)))
      (setq first-pos (string-match " - 1st" (buffer-string)))
      (should (equal nil (do-done-position)))
      (should (< second-pos first-pos))
      (should (= (point) (+ 1 first-pos)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2240-do-task-down ()
  "do-task-down: two tasks, above DONE -- top moves past bottom"
  (with-temp-buffer
    (let ((before) (first-pos) (second-pos) (done-pos))
      (insert "\n\n - 1st task\n\n - 2nd task\n\n" g-done-line)
      (setq before (buffer-string))
      (goto-char (string-match "1st" (buffer-string)))
      (do-task-down)                    ; payload
      (setq second-pos (string-match " - 2nd" (buffer-string)))
      (setq first-pos (string-match " - 1st" (buffer-string)))
      (setq done-pos (do-done-position))
      (should (< second-pos first-pos))
      (should (< first-pos done-pos))
      (should (= (point) (+ 1 first-pos)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2245-do-task-down ()
  "do-task-down: two tasks, below DONE -- DONE won't move past top"
  (with-temp-buffer
    (let ((before))
      (insert g-done-line "\n\n < 1st task\n\n + 2nd task\n\n")
      (setq before (buffer-string))
      (goto-char (+ 5 (do-done-position)))
      (do-task-down)                     ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2250-do-task-down ()
  "do-task-down: two tasks, below DONE -- top moves past bottom"
  (with-temp-buffer
    (let ((before) (first-pos) (second-pos))
      (insert g-done-line "\n\n + 1st task\n\n + 2nd task\n\n")
      (setq before (buffer-string))
      (goto-char (string-match-end g-first (buffer-string)))
      (do-task-down)                     ; payload
      (setq first-pos (string-match " \\+ 1st" (buffer-string)))
      (setq second-pos (string-match " \\+ 2nd" (buffer-string)))
      (should (< second-pos first-pos))
      (should (< (do-done-position) second-pos))
      (should (= (point) (+ 1 first-pos)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2255-do-task-down ()
  "do-task-down: two tasks, above DONE -- bottom won't move past DONE"
  (with-temp-buffer
    (let ((before))
      (insert "\n\n - 1st task\n\n - 2nd task\n" g-done-line)
      (setq before (buffer-string))
      (goto-char (string-match-end g-second (buffer-string)))
      (do-task-down)                     ; payload
      (should (string= before (buffer-string)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-2260-do-task-down ()
  "do-task-down: three tasks, DONE after 1 -- downing 2nd should
not take DONE with it"
  (with-temp-buffer
    (let ((first-pos) (second-pos) (done-pos) (third-pos))
      (insert "\n\n - 1st task\n\n" g-done-line "\n\n + 2nd task\n\n + 3rd task")
      (goto-char (string-match-end "2nd task" (buffer-string)))
      (do-task-down)                     ; payload
      (setq first-pos (string-match " - 1st task" (buffer-string)))
      (setq done-pos (string-match "--- DONE ---" (buffer-string)))
      (setq third-pos (string-match " \\+ 3rd task" (buffer-string)))
      (setq second-pos (string-match " \\+ 2nd task" (buffer-string)))
      (should (< first-pos done-pos))
      (should (< done-pos third-pos))
      (should (< third-pos second-pos))
      (should (= (point) (+ 1 second-pos)))
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

(ert-deftest test-2400-task-to-top ()
  "do-task-to-top: In a zero-length file, this will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2405-task-to-top ()
  "do-task-to-top: In a file of whitespace, this will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2410-task-to-top ()
  "do-task-to-top: In a file with one task and no DONE line, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2415-task-to-top ()
  "do-task-to-top: In a file with a DONE line and one task above, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2420-task-to-top ()
  "do-task-to-top: In a file with a DONE line and one task below, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2425-task-to-top ()
  "do-task-to-top: In a file with no DONE line and two tasks, this
will not move the top task (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2430-task-to-top ()
  "do-task-to-top: In a file with no DONE line and two tasks, this
will move the bottom task to top."
  (with-temp-buffer
    (let ((first-pos) (second-pos))
      (insert g-two-tasks)
      (goto-char (string-match-end g-second (buffer-string)))
      (do-task-to-top)
      (setq first-pos (string-match g-1st-task (buffer-string)))
      (setq second-pos (string-match g-2nd-task (buffer-string)))
      (should (< second-pos first-pos))
      )))

(ert-deftest test-2435-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks above, this
will not move the top task (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2440-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks above, this
will move the bottom task to top."
  (with-temp-buffer
    (let ((first-pos) (second-pos) (done-pos))
      (insert g-two-tasks g-done-line)
      (goto-char (string-match-end g-2nd-task (buffer-string)))
      (do-task-to-top)
      (setq first-pos (string-match g-1st-task (buffer-string)))
      (setq second-pos (string-match g-2nd-task (buffer-string)))
      (setq done-pos (do-done-position))
      (should (< second-pos first-pos))
      (should (< first-pos done-pos))
      )))

(ert-deftest test-2445-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks below, this
will not move the top task (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2450-task-to-top ()
  "do-task-to-top: In a file with a DONE line and two tasks below, this
will move the bottom task to just below DONE line."
  (with-temp-buffer
    (let ((done-pos) (first-pos) (second-pos))
      (insert g-done-line g-plus-two-tasks)
      (goto-char (string-match-end "2nd task" (buffer-string)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match "1st task" (buffer-string)))
      (setq second-pos (string-match "2nd task" (buffer-string)))
      (should (< done-pos second-pos))
      (should (< second-pos first-pos))
      )))

(ert-deftest test-2455-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will not move the top task above the DONE line (no
changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2460-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the middle task above the
DONE line to the beginning of the file."
  (with-temp-buffer
    (let ((first-pos) (second-pos) (third-pos) (done-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " - 2nd samp" (buffer-string)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match " - 1st sample" (buffer-string)))
      (setq second-pos (string-match " - 2nd sample" (buffer-string)))
      (setq third-pos (string-match " - 3rd sample" (buffer-string)))
      (should (< second-pos first-pos))
      (should (< first-pos third-pos))
      (should (< third-pos done-pos))
      )))

(ert-deftest test-2465-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the bottom task above the
DONE line to the top of the file."
  (with-temp-buffer
    (let ((first-pos) (second-pos) (third-pos) (done-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " - 3rd samp" (buffer-string)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match " - 1st sample" (buffer-string)))
      (setq second-pos (string-match " - 2nd sample" (buffer-string)))
      (setq third-pos (string-match " - 3rd sample" (buffer-string)))
      (should (< third-pos first-pos))
      (should (< first-pos second-pos))
      (should (< second-pos done-pos))
      )))

(ert-deftest test-2470-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will not move the top task below the
DONE line (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-top)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2475-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the middle task below the
DONE line to just below the DONE line."
  (with-temp-buffer
    (let ((first-pos) (second-pos) (third-pos) (done-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " \\+ 2nd samp" (buffer-string)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match " \\+ 1st sample" (buffer-string)))
      (setq second-pos (string-match " \\+ 2nd sample" (buffer-string)))
      (setq third-pos (string-match " \\+ 3rd sample" (buffer-string)))
      (should (< done-pos second-pos))
      (should (< second-pos first-pos))
      (should (< first-pos third-pos))
      )))

(ert-deftest test-2480-task-to-top ()
  "do-task-to-top: In a file with a DONE line and three tasks
above and three below, this will move the bottom task below the
DONE line to just below the DONE line."
  (with-temp-buffer
    (let ((first-pos) (second-pos) (third-pos) (done-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " \\+ 3rd samp" (buffer-string)))
      (do-task-to-top)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match " \\+ 1st sample" (buffer-string)))
      (setq second-pos (string-match " \\+ 2nd sample" (buffer-string)))
      (setq third-pos (string-match " \\+ 3rd sample" (buffer-string)))
      (should (< done-pos third-pos))
      (should (< third-pos first-pos))
      (should (< first-pos second-pos))
      )))

;; ============================================================================
;; tests for do-task-to-end

(ert-deftest test-2500-task-to-end ()
  "do-task-to-end: In a zero-length file, this will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2505-task-to-end ()
  "do-task-to-end: In a file of whitespace, this will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2510-task-to-end ()
  "do-task-to-end: In a file with one task and no DONE line, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2515-task-to-end ()
  "do-task-to-end: In a file with a DONE line and one task above, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2520-task-to-end ()
  "do-task-to-end: In a file with a DONE line and one task below, this
will make no changes."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2525-task-to-end ()
  "do-task-to-end: In a file with no DONE line and two tasks, this
will not move the bottom task (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2530-task-to-end ()
  "do-task-to-end: In a file with no DONE line and two tasks, this
will move the top task to the end."
  (with-temp-buffer
    (let ((first-pos) (second-pos))
      (insert g-two-tasks)
      (goto-char (string-match-end g-first (buffer-string)))
      (do-task-to-end)
      (setq first-pos (string-match g-1st-task (buffer-string)))
      (setq second-pos (string-match g-2nd-task (buffer-string)))
      (should (< second-pos first-pos))
      )))

(ert-deftest test-2535-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks above, this
will not move the bottom task (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2540-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks above, this
will move the top task to just before the DONE line."
  (with-temp-buffer
    (let ((done-pos) (first-pos) (second-pos))
      (insert g-two-tasks g-done-line)
      (goto-char (string-match-end g-1st-task (buffer-string)))
      (do-task-to-end)
      (setq first-pos (string-match g-1st-task (buffer-string)))
      (setq second-pos (string-match g-2nd-task (buffer-string)))
      (setq done-pos (do-done-position))
      (should (< second-pos first-pos))
      (should (< first-pos done-pos))
      )))

(ert-deftest test-2545-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks below, this
will not move the bottom task (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2550-task-to-end ()
  "do-task-to-end: In a file with a DONE line and two tasks below, this
will move the top task to the end of the file."
  (with-temp-buffer
    (let ((done-pos) (first-pos) (second-pos))
      (insert g-done-line g-plus-two-tasks)
      (goto-char (string-match-end "1st task" (buffer-string)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match "1st task" (buffer-string)))
      (setq second-pos (string-match "2nd task" (buffer-string)))
      (should (< done-pos second-pos))
      (should (< second-pos first-pos))
      )))

(ert-deftest test-2555-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will not move the bottom task above the DONE line (no
changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2560-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the middle task above the
DONE line to just before the DONE line."
  (with-temp-buffer
    (let ((done-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " - 2nd samp" (buffer-string)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match " - 1st sample" (buffer-string)))
      (setq second-pos (string-match " - 2nd sample" (buffer-string)))
      (setq third-pos (string-match " - 3rd sample" (buffer-string)))
      (should (< first-pos third-pos))
      (should (< third-pos second-pos))
      (should (< second-pos done-pos))
      )))

(ert-deftest test-2565-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the top task above the
DONE line to just before the DONE line."
  (with-temp-buffer
    (let ((done-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " - 1st samp" (buffer-string)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match " - 1st sample" (buffer-string)))
      (setq second-pos (string-match " - 2nd sample" (buffer-string)))
      (setq third-pos (string-match " - 3rd sample" (buffer-string)))
      (should (< second-pos third-pos))
      (should (< third-pos first-pos))
      (should (< first-pos done-pos))
      )))

(ert-deftest test-2570-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will not move the bottom task below the
DONE line (no changes)."
  (with-temp-buffer
    (let ((before))
      (setq before (buffer-string))
      (do-task-to-end)
      (should (string= before (buffer-string)))
      )))

(ert-deftest test-2575-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the middle task below the
DONE line to the end of the file."
  (with-temp-buffer
    (let ((done-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " \\+ 2nd samp" (buffer-string)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (setq first-pos (string-match " \\+ 1st sample" (buffer-string)))
      (setq third-pos (string-match " \\+ 3rd sample" (buffer-string)))
      (setq second-pos (string-match " \\+ 2nd sample" (buffer-string)))
      (should (< done-pos first-pos))
      (should (< first-pos third-pos))
      (should (< third-pos second-pos))
      )))

(ert-deftest test-2580-task-to-end ()
  "do-task-to-end: In a file with a DONE line and three tasks
above and three below, this will move the top task below the
DONE line to the end of the file."
  (with-temp-buffer
    (let ((done-pos) (first-pos) (second-pos) (third-pos))
      (insert g-buf-samples3 g-done-line
              (replace-regexp-in-string "-" "+" g-buf-samples3))
      (goto-char (string-match-end " \\+ 1st samp" (buffer-string)))
      (do-task-to-end)
      (setq done-pos (do-done-position))
      (setq second-pos (string-match " \\+ 2nd sample" (buffer-string)))
      (setq third-pos (string-match " \\+ 3rd sample" (buffer-string)))
      (setq first-pos (string-match " \\+ 1st sample" (buffer-string)))
      (should (< done-pos second-pos))
      (should (< second-pos third-pos))
      (should (< third-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
;; Copy this to *scratch* and eval-buffer (esc-b) to run the tests
;; interactively
;;
;; (reload-do-mode)
;; (ert-run-tests-interactively "t")
