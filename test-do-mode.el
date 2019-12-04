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
;;    do-add-done-iff     (1000 - 1099)
;;    do-done-position    (1100 - 1199)
;;    do-goto-next-task   (1200 - 1399)
;;    do-goto-prev-task   (1400 - 1599)
;;    do-new-entry        (1600 - 1699)
;;    do-next-task-mark   (1700 - 1799)
;;    do-prev-task-mark   (1800 - 1899)
;;    do-[pxo]done        (1900 - 1999)
;;


;; ============================================================================
;; variables
(setq 2nd-sample "2nd sample")
(setq 3rd-sample "3rd sample")
(setq abandoned "\n\n x abandoned task number 2\n")
(setq abandoned-1st "---\n\n x 1st")
(setq also-sp "also ")
(setq buf-no-done "      \n\n - first task\n - second task\n")
(setq buf-w-done
      (concat "    \n\n\n"                ;  1 -  7
              " - task one\n\n"           ;  8 - 20
              " - task two  \n\n"         ; 21 - 35
              " - task three\n\n"         ; 36 - 50
              "--- DONE --------------------------------------------\n\n"
              " + finished 1\n\n"
              " + also done 2\n"))
(setq buf-samples1 (concat "\n\n - single sample task\n\n"))
(setq buf-samples2 (concat "\n\n - 1st sample task"
                           "\n\n - 2nd sample task"
                           "\n\n"))
(setq buf-samples3 (concat "\n\n - 1st sample task"
                           "\n\n - 2nd sample task"
                           "\n\n - 3rd sample task"
                           "\n\n"))
(setq completed "\n\n + completed task number 1")
(setq dash-1st-sample "- 1st sample")
(setq dash-2nd-sample " - 2nd sample")
(setq dash-3rd-sample " - 3rd sample")
(setq dash-first " - first")
(setq dash-singsamp " - single sample")
(setq dash-sp-f "- f")
(setq dash-sp-s "- s")
(setq dash-sp-t "- t")
(setq dash-tas " - tas")
(setq dash-task-thr " - task thr")
(setq diverted-2nd "---\n\n < 2nd sample")
(setq done-line "--- DONE --------------------------------------------")
(setq done-sample "---\n\n \\+ sample")
(setq e-new-new "e\n\n")
(setq file-too-small "file too small to hold a task")
(setq finis "finis")
(setq inish "inish")
(setq k-sp-o "k o")
(setq less-2nd-sample " < 2nd sample")
(setq less-3rd-sample " < 3rd sample")
(setq n-e-sp-2-new "ne 2\n")
(setq new-new-sp "\n\n ")
(setq new3-dash "\n\n\n -")
(setq new-sp-dash "\n -")
(setq new-task-rgx " - \\[[.0-9]\\{9\\}\\] ")
(setq no-active-tasks "no active tasks found")
(setq no-tasks "no tasks in file")
(setq plus-1st-sample " \\+ 1st sample")
(setq plus-3rd-sample " \\+ 3rd sample")
(setq plus-also "+ also")
(setq plus-fini " + fini")
(setq plus-sample " \\+ sample")
(setq plus-single " \\+ single")
(setq plus-sp-f "+ f")
(setq s-k-new "sk\n")
(setq sample "sample")
(setq sample-task "\n\n - sample task\n\n")
(setq six-new "\n\n\n\n\n\n")
(setq sp-also-sp " also ")
(setq sp-dash-sp " - ")
(setq sp-first " 1st")
(setq sp-plus-also " + also")
(setq sp-plus-sp " + ")
(setq sp-f-i " fi")
(setq sp-fini " fini")
(setq sp-new-new " \n\n")
(setq sp-s-e " se")
(setq sp-second " 2nd")
(setq sp-t-a " ta")
(setq t-a-s "tas")
(setq t-sp-t "t t")
(setq three-new "\n\n\n")
(setq three-sp "   ")
(setq whitespace "       \n         \n              \n         ")
(setq x-first " x 1st")
(setq x-second-sample " x 2nd sample")

;; ============================================================================
;; helper functions

;; ----------------------------------------------------------------------------
(defun bytes-at (where count)
  "Return the next COUNT bytes after point (or before point at eobp)"
  (buffer-substring (min where (- (point-max) count))
                    (min (+ count where) (point-max))))

;; ----------------------------------------------------------------------------
(defun get-message-max ()
  "Empty the *Messages* buffer"
  (with-current-buffer "*Messages*"
    (point-max)))

;; ----------------------------------------------------------------------------
(defun in-messages-p (after needle)
  "If NEEDLE is in buffer *Messages* after AFTER, return its index, otherwise nil"
  (with-current-buffer "*Messages*"
    (string-match needle (buffer-substring after (point-max)))))

;; ----------------------------------------------------------------------------
(defun last-position (target &optional before)
  "Return the position of the last occurrence of TARGET in the buffer"
  (save-excursion
    (if before
        (goto-char before)
      (goto-char (point-max)))
    (re-search-backward target)))

;; ----------------------------------------------------------------------------
(defun run-tests ()
  "Set variable test-selector and return"
  (interactive)
  (ert-run-tests-batch-and-exit selector))


;; ============================================================================
;; tests for do-add-done-iff

;; ----------------------------------------------------------------------------
(ert-deftest test-1000-add-done-iff-empty ()
  (concat "verify that do-add-done-iff adds a done line if there isn't"
          " one in the buffer already -- empty")
  (with-temp-buffer
    (do-add-done-iff)
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1010-add-done-iff-absent-w-tasks ()
  (concat "verify that do-add-done-iff adds a done line if there isn't"
          " one in the buffer already -- with tasks")
  (with-temp-buffer
    (let ((done-pos)
          (ltask-pos))
      (insert buf-no-done)
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
  (concat "verify that do-add-done-iff doesn't add a done line"
          " if one is already present")
  (with-temp-buffer
    (insert three-new)
    (insert do-mode-done-line)
    (insert six-new)
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))
    (do-add-done-iff)
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))
    (should (equal nil (re-search-forward do-mode-rgx-done nil 't)))))


;; ============================================================================
;; tests for do-done-position

;; ----------------------------------------------------------------------------
(ert-deftest test-1100-do-done-position-no-done ()
  "test do-done-position when DONE line is missing"
  (with-temp-buffer
    (insert six-new)
    (should (equal nil (do-done-position)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1110-do-done-position-with-done ()
  "test do-done-position when DONE line is present"
  (with-temp-buffer
    (insert three-sp three-new done-line three-new)
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
    (insert buf-no-done)
    (goto-char 2)
    (should (= 9 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1215-next-no-done ()
  "goto-next: no DONE, two tasks, point just before first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 7)
    (should (string= new-new-sp (bytes-at (point) 3)))
    (should (= 9 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1220-next-no-done ()
  "goto-next: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 9)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1230-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 10)
    (should (string= dash-sp-f (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1240-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 11)
    (should (string= sp-f-i (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1250-next-no-done ()
  "goto-next: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 16)
    (should (string= t-sp-t (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1255-next-no-done ()
  "goto-next: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 22)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1260-next-no-done ()
  "goto-next: no DONE, two tasks, point at second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 23)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1270-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 24)
    (should (string= dash-sp-s (bytes-at (point) 3)))
    (should (= 24 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1280-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 25)
    (should (string= sp-s-e (bytes-at (point) 3)))
    (should (= (point) (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1290-next-no-done ()
  "goto-next: no DONE, two tasks, point near end of second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char (point-max))
    (should (string= s-k-new (bytes-at (- (point-max) 3) 3)))
    (should (= (point-max) (do-goto-next-task)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1310-next-w-done ()
  "goto-next: with DONE, four tasks, point at start of buffer"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 1)
    (should (string= three-sp (bytes-at (point) 3)))
    (should (= 8 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1320-next-w-done ()
  "goto-next: with DONE, four tasks, point just before 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 7)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 8 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1330-next-w-done ()
  "goto-next: with DONE, four tasks, point at 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 8)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1340-next-w-done ()
  "goto-next: with DONE, four tasks, point one byte into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 9)
    (should (string= dash-sp-t (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1350-next-w-done ()
  "goto-next: with DONE, four tasks, point two bytes into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 10)
    (should (string= sp-t-a (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1360-next-w-done ()
  "goto-next:  with DONE, four tasks, point in middle of 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 14)
    (should (string= k-sp-o (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1370-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 20)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1380-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 21)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1390-next-w-done ()
  "goto-next:  with DONE, four tasks, point a couple bytes into 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 24)
    (should (string= t-a-s (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1392-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 33)
    (should (string= sp-new-new (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1394-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 35)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))
    (should (string= sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1396-next-w-done ()
  "goto-next:  with DONE, four tasks, point at end of 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 48)
    (should (string= e-new-new (bytes-at (point) 3)))
    (should (= 106 (do-goto-next-task)))
    (should (string= sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1398-next-w-done ()
  "goto-next:  with DONE, four tasks, point in 4th (completed) task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 107)
    (should (string= plus-sp-f (bytes-at (point) 3)))
    (should (= 121 (do-goto-next-task)))
    (should (string= sp-plus-sp (bytes-at 106 3)))
    (should (string= sp-plus-sp (bytes-at 121 3)))))

;; ============================================================================
;; tests for do-goto-prev-task
;;
;; We'll start at the bottom of the buffer and work backwards across
;; the DONE line if it's there

;; ----------------------------------------------------------------------------
(ert-deftest test-1400-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char (point-max))
    (should (string= n-e-sp-2-new (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 121 result))
    (should (string= sp-plus-also (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1405-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 124)
    (should (string= also-sp (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 121 result))
    (should (string= sp-plus-also (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1410-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 123)
    (should (string= sp-also-sp (bytes-at (point) 6)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= plus-fini (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1420-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task by 1 byte"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 122)
    (should (string= plus-also (bytes-at (point) 6)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= plus-fini (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1430-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at last task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 121)
    (should (= 121 (point)))
    (should (string= sp-plus-also (bytes-at (point) 7)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= plus-fini (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1440-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 110)
    (should (string= inish (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= plus-fini (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1442-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point early in penultimate task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 109)
    (should (string= finis (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= plus-fini (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1444-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task mark"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 108)
    (should (string= sp-fini (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 36 result))
    (should (string= dash-task-thr (bytes-at result 11)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1450-prev-w-done ()
  "prev task: with DONE, 5 tasks, point before 1st task, land on 1st"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 5)
    (should (string= new3-dash (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 8 result))
    (should (string= dash-tas (bytes-at result 6)))))

;; ============================================================================
;; tests for do-new-entry
;;

;; ----------------------------------------------------------------------------
(ert-deftest test-1600-new-001 ()
  "new entry: empty file"
  (with-temp-buffer
    (do-new-task)                           ; payload
    (should (string-match new-task-rgx (buffer-string)))
    ))

;; ----------------------------------------------------------------------------
(ert-deftest test-1605-new-002 ()
  "new entry: no DONE line, one task, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (otask-pos))
      (insert buf-samples1)
      (goto-char (point-min))
      (do-new-task)                                                   ; payload
      (goto-char (point-min))
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq otask-pos
                    (string-match " - single sample" (buffer-string))))
      (should (< ntask-pos otask-pos))
    )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1610-new-003 ()
  "new entry: no DONE line, one task, after 1st"
  (with-temp-buffer
    (let ((ntask-pos) (otask-pos))
      (insert buf-samples1)
      (goto-char (string-match "task" (buffer-string)))
      (do-new-task)                                                   ; payload
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq otask-pos (string-match dash-singsamp (buffer-string))))
      (should (< otask-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1615-new-004 ()
  "new entry: no DONE line, two tasks, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert buf-samples2)
      (goto-char (point-min))
      (do-new-task)                                                   ; payload
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match " - 1st" (buffer-string))))
      (should (setq second-pos (string-match " - 2nd" (buffer-string))))
      (should (< ntask-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1620-new-005 ()
  "new entry: no DONE line, two tasks, before 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert buf-samples2)
      (goto-char (- (string-match " - 2nd sample" (buffer-string)) 5))
      (do-new-task)                                                   ; payload
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match " - 1st" (buffer-string))))
      (should (setq second-pos (string-match " - 2nd" (buffer-string))))
      (should (< first-pos ntask-pos))
      (should (< ntask-pos second-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1625-new-006 ()
  "new entry: no DONE line, two tasks, after 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos))
      (insert buf-samples2)
      (goto-char (- (point-max) 1))
      (do-new-task)
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match " - 1st" (buffer-string))))
      (should (setq second-pos (string-match " - 2nd" (buffer-string))))
      (should (< first-pos ntask-pos))
      (should (< second-pos ntask-pos))
    )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1630-new-007 ()
  "new entry: no DONE line, three tasks, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert buf-samples3)
      (goto-char 2)
      (do-new-task)
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match " - 1st" (buffer-string))))
      (should (setq second-pos (string-match " - 2nd" (buffer-string))))
      (should (setq third-pos (string-match " - 3rd" (buffer-string))))
      (should (< ntask-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1635-new-008 ()
  "new entry: no DONE line, three tasks, before 2nd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert buf-samples3)
      (goto-char 3)
      (end-of-line)
      (do-new-task)
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match " - 1st" (buffer-string))))
      (should (setq second-pos (string-match " - 2nd" (buffer-string))))
      (should (setq third-pos (string-match " - 3rd" (buffer-string))))
      (should (< first-pos ntask-pos))
      (should (< ntask-pos second-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1640-new-009 ()
  "new entry: no DONE line, three tasks, before 3rd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert buf-samples3)
      (goto-char (+ (string-match "2nd sample" (buffer-string)) 10))
      (do-new-task)
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match " - 1st" (buffer-string))))
      (should (setq second-pos (string-match " - 2nd" (buffer-string))))
      (should (setq third-pos (string-match " - 3rd" (buffer-string))))
      (should (< second-pos ntask-pos))
      (should (< ntask-pos third-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1645-new-010 ()
  "new entry: no DONE line, three tasks, after 3rd"
  (with-temp-buffer
    (let ((ntask-pos) (first-pos) (second-pos) (third-pos))
      (insert buf-samples3)
      (goto-char (+ (string-match "3rd sample" (buffer-string)) 10))
      (do-new-task)
      (should (setq ntask-pos (string-match new-task-rgx (buffer-string))))
      (should (setq first-pos (string-match " - 1st" (buffer-string))))
      (should (setq second-pos (string-match " - 2nd" (buffer-string))))
      (should (setq third-pos (string-match " - 3rd" (buffer-string))))
      (should (< third-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1650-new-011 ()
  "new entry: DONE line present, no tasks"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert done-line)
      (goto-char (point-min))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match new-task-rgx (buffer-string)))
      (should (< ntask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1655-new-011 ()
  "new entry: DONE line present, no tasks"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert done-line)
      (goto-char (point-max))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match new-task-rgx (buffer-string)))
      (should (< ntask-pos done-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1660-new-012 ()
  "new entry: DONE line present, one task, before 1st"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert buf-samples1 done-line)
      (goto-char (point-min))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match new-task-rgx (buffer-string)))
      (setq first-pos (string-match " - single sample" (buffer-string)))
      (should (< ntask-pos done-pos))
      (should (< ntask-pos first-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1665-new-012 ()
  "new entry: DONE line present, one task, after 1st"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos) (first-pos))
      (insert buf-samples1 done-line "\n\n")
      (goto-char (string-match "task" (buffer-string)))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match new-task-rgx (buffer-string)))
      (setq first-pos (string-match " - single sample" (buffer-string)))
      (should (< ntask-pos done-pos))
      (should (< first-pos ntask-pos))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1670-new-012 ()
  "new entry: DONE line present, one task, after done line"
  (with-temp-buffer
    (let ((ntask-pos) (done-pos))
      (insert buf-samples1 done-line "\n\n")
      (goto-char (point-max))
      (do-new-task)
      (setq done-pos (do-done-position))
      (setq ntask-pos (string-match new-task-rgx (buffer-string)))
      (setq first-pos (string-match " - single sample" (buffer-string)))
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
    (insert buf-no-done)
    (goto-char 2)
    (should (= 9 (do-next-task-mark)))
    (should (string= dash-first (bytes-at 9 8)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1703-ntm-no-done ()
  "next-task: no DONE, two tasks, point just before first task"
  (with-temp-buffer
    (let ((result))
      (insert buf-no-done)
      (goto-char 7)
      (should (string= new-new-sp (bytes-at (point) 3)))
      (setq result (do-next-task-mark))
      (should (= 9 result))
      (should (string= dash-first (bytes-at result 8))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1706-ntm-no-done ()
  "next-task: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 9)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1709-ntm-no-done ()
  "next-task: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 10)
    (should (string= dash-sp-f (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1712-ntm-no-done ()
  "next-task: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 11)
    (should (string= sp-f-i (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1715-ntm-no-done ()
  "next-task: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 16)
    (should (string= t-sp-t (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1718-ntm-no-done ()
  "next-task: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 22)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1721-ntm-no-done ()
  "next-task: no DONE, two tasks, point at second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 23)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1724-ntm-no-done ()
  "next-task: no DONE, two tasks, point just inside second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 24)
    (should (string= dash-sp-s (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1727-ntm-no-done ()
  "next-task: no DONE, two tasks, point two bytes into second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 25)
    (should (string= sp-s-e (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1730-ntm-no-done ()
  "next-task: no DONE, two tasks, point near end of second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char (point-max))
    (should (string= s-k-new (bytes-at (point) 3)))
    (should (equal nil (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1733-ntm-w-done ()
  "next-task: with DONE, four tasks, point at start of buffer"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 1)
    (should (string= three-sp (bytes-at (point) 3)))
    (should (= 8 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1736-ntm-w-done ()
  "next-task: with DONE, four tasks, point just before 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 7)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 8 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1739-ntm-w-done ()
  "next-task: with DONE, four tasks, point at 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 8)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1742-ntm-w-done ()
  "next-task: with DONE, four tasks, point one byte into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 9)
    (should (string= dash-sp-t (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1745-ntm-w-done ()
  "next-task: with DONE, four tasks, point two bytes into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 10)
    (should (string= sp-t-a (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1748-ntm-w-done ()
  "next-task:  with DONE, four tasks, point in middle of 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 14)
    (should (string= k-sp-o (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1751-ntm-w-done ()
  "next-task:  with DONE, four tasks, point just before 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 20)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1754-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 21)
    (should (string= sp-dash-sp (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1757-ntm-w-done ()
  "next-task:  with DONE, four tasks, point a couple bytes into 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 24)
    (should (string= t-a-s (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1760-ntm-w-done ()
  "next-task:  with DONE, four tasks, point just before 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 33)
    (should (string= sp-new-new (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1763-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 35)
    (should (string= new-sp-dash (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))
    (should (string= sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1766-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at end of 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 48)
    (should (string= e-new-new (bytes-at (point) 3)))
    (should (= 106 (do-next-task-mark)))
    (should (string= sp-plus-sp (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1769-ntm-w-done ()
  "next-task:  with DONE, four tasks, point in 4th (completed) task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 107)
    (should (string= plus-sp-f (bytes-at (point) 3)))
    (should (= 121 (do-next-task-mark)))
    (should (string= sp-plus-sp (bytes-at 106 3)))
    (should (string= sp-plus-sp (bytes-at 121 3)))))

;; ============================================================================
;; tests for do-prev-task-mark
;;

;; ----------------------------------------------------------------------------
(ert-deftest test-1800-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char (point-max))
      (should (string= n-e-sp-2-new (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 121 result))
      (should (string= sp-plus-also (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1805-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 124)
      (should (string= also-sp (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 121 result))
      (should (string= sp-plus-also (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1810-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in last task"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 123)
      (should (string= sp-also-sp (bytes-at (point) 6)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1815-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in last task by 1 byte"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 122)
      (should (string= plus-also (bytes-at (point) 6)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1820-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at last task"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 121)
      (should (= 121 (point)))
      (should (string= sp-plus-also (bytes-at (point) 7)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1825-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in penultimate task"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 110)
      (should (string= inish (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1830-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point early in penultimate task"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 109)
      (should (string= finis (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= plus-fini (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1835-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in penultimate task mark"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 108)
      (should (string= sp-fini (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 36 result))
      (should (string= dash-task-thr (bytes-at result 11))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1840-ptm-w-done ()
  "prev task: with DONE, 5 tasks, point before 1st task, land on 1st"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 5)
      (should (string= new3-dash (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 8 result))
      (should (string= dash-tas (bytes-at result 6))))))

;; ============================================================================
;; tests for do-[pxo]done

;; ----------------------------------------------------------------------------
(ert-deftest test-1900-pdone-empty ()
  "pdone: empty file doesn't change"
  (with-temp-buffer
    (let ((msg-max (get-message-max)))

      (do-pdone 't)                                                   ; payload

      (should (in-messages-p msg-max file-too-small))
      (should (= (point-max) 1)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1905-xdone-whitespace ()
  "xdone: file of whitespace doesn't change"
  (with-temp-buffer
    (let ((msg-max (get-message-max))
          (before)
          (pre-point))
      (insert whitespace)
      (setq before (buffer-string))
      (setq pre-point (point))

      (do-xdone 't)                                                   ; payload

      (should (in-messages-p msg-max no-tasks))
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
      (insert completed)
      (insert abandoned)
      (setq before (buffer-string))
      (goto-char (point-max))
      (setq pre-point (point))

      (do-odone 't)                                                   ; payload

      (should (in-messages-p msg-max no-active-tasks))
      (should (string= before (buffer-string)))
      (should (= pre-point (point)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1915-pdone-ndl-one ()
  "pdone: one task, no DONE -> DONE line added, task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert sample-task)
      (goto-char (string-match sample (buffer-string)))

      (do-pdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (goto-char (point-max))
      (setq task-pos (re-search-backward do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position plus-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1920-xdone-ndl-two-1st ()
  "two task, no DONE: DONE line added, first task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert buf-samples2)
      (goto-char (string-match sp-first (buffer-string)))

      (do-xdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position x-first)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1925-odone-ndl-two-2nd ()
  "two task, no DONE: DONE line added, second task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert buf-samples2)
      (goto-char (string-match sp-second (buffer-string)))

      (do-odone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position less-2nd-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1930-pdone-ndl-three-1st ()
  "three tasks, no DONE: DONE line added, first task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert buf-samples3)
      (goto-char (string-match dash-1st-sample (buffer-string)))

      (do-pdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position plus-1st-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1935-xdone-ndl-three-2nd ()
  "three tasks, no DONE: DONE line added, middle task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert buf-samples3)
      (goto-char (string-match 2nd-sample (buffer-string)))

      (do-xdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position x-second-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1940-odone-ndl-three-3rd ()
  "three tasks, no DONE: DONE line added, last task moves below"
  (with-temp-buffer
    (let ((done-pos)
          (task-pos))
      (insert buf-samples3)
      (goto-char (string-match 3rd-sample (buffer-string)))

      (do-odone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position less-3rd-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1945-pdone-wdl-one ()
  "one task, with DONE: task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert buf-samples1 done-line)
      (goto-char (string-match sample (buffer-string)))

      (do-pdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position plus-single)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1950-xdone-wdl-two-1st ()
  "two tasks, with DONE: first task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert buf-samples2 done-line)
      (goto-char (string-match sample (buffer-string)))

      (do-xdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position x-first)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1955-odone-wdl-two-2nd ()
  "two tasks, with DONE: second task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert buf-samples2 done-line)
      (goto-char (string-match 2nd-sample (buffer-string)))

      (do-odone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position less-2nd-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1960-pdone-wdl-three-1st ()
  "three tasks, with DONE: 1st task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert buf-samples3 done-line)
      (goto-char (string-match sp-first (buffer-string)))

      (do-pdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position plus-1st-sample)))
  )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1965-xdone-wdl-three-2nd ()
  "three tasks, with DONE: 2nd task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert buf-samples3 done-line)
      (goto-char (string-match 2nd-sample (buffer-string)))

      (do-xdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position x-second-sample)))
      )))

;; ----------------------------------------------------------------------------
(ert-deftest test-1970-odone-wdl-three-3rd ()
  "three tasks, with DONE: 3rd task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert buf-samples3 done-line)
      (goto-char (string-match "- 3rd sample" (buffer-string)))

      (do-odone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq task-pos (last-position do-mode-rgx-task))
      (should (< done-pos task-pos))
      (should (= task-pos (last-position less-3rd-sample)))
      )))

;; ----------------------------------------------------------------------------
;;  three tasks, with DONE: 1st, 2nd move below without concatenation
(ert-deftest test-1975-odone-wdl-three-3rd ()
  "three tasks, with DONE: 3rd task moves below"
  (with-temp-buffer
    (let ((done-pos) (task-pos))
      (insert buf-samples3 done-line)
      (goto-char (string-match "- 2nd sample" (buffer-string)))

      (do-odone 't)                                                   ; payload
      (do-pdone 't)                                                   ; payload

      (setq done-pos (do-done-position))
      (setq ltask-pos (last-position do-mode-rgx-task))
      (setq ptask-pos (last-position do-mode-rgx-task ltask-pos))
      (should (< ptask-pos ltask-pos))
      (should (< done-pos ptask-pos))
      (should (= ltask-pos (last-position less-2nd-sample)))
      (should (= ptask-pos (last-position plus-3rd-sample ltask-pos)))
      (should (equal nil (string-match dash-3rd-sample (buffer-string))))
      )))

;; ----------------------------------------------------------------------------
;; Copy this to *scratch* and eval-buffer (esc-b) to run the tests
;; interactively
;;
;; (reload-do-mode)
;; (ert-run-tests-interactively "t")
