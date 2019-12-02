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
;;


;; ============================================================================
;; variables
(setq buf-no-done "      \n\n - first task\n - second task\n")
(setq buf-w-done
      (concat "    \n\n\n"                ;  1 -  7
              " - task one\n\n"           ;  8 - 20
              " - task two  \n\n"         ; 21 - 35
              " - task three\n\n"         ; 36 - 50
              "--- DONE --------------------------------------------\n\n"
              " + finished 1\n\n"
              " + also done 2\n"))

;; ============================================================================
;; helper functions

;; ----------------------------------------------------------------------------
(defun bytes-at (where count)
  "Return the next COUNT bytes after point (or before point at eobp)"
  (buffer-substring (min where (- (point-max) count))
                    (min (+ count where) (point-max))))


;; ============================================================================
;; tests for do-add-done-iff

;; ----------------------------------------------------------------------------
(ert-deftest test-1000-add-done-iff-absent ()
  (concat "verify that do-add-done-iff adds a done line if there isn't"
          " one in the buffer already")
  (with-temp-buffer
    (do-add-done-iff)
    (goto-char (point-min))
    (should (re-search-forward do-mode-rgx-done))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1001-add-done-iff-present ()
  (concat "verify that do-add-done-iff doesn't add a done line"
          " if one is already present")
  (with-temp-buffer
    (insert "\n\n\n")
    (insert do-mode-done-line)
    (insert "\n\n\n\n\n\n")
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
    (insert "\n\n\n\n\n")
    (should (= (point-max) (do-done-position)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1110-do-done-position-with-done ()
  "test do-done-position when DONE line is present"
  (with-temp-buffer
    (insert "    \n\n--- DONE -------------------------\n\n\n")
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
    (should (string= "\n\n " (bytes-at (point) 3)))
    (should (= 9 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1220-next-no-done ()
  "goto-next: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 9)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1230-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 10)
    (should (string= "- f" (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1240-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 11)
    (should (string= " fi" (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1250-next-no-done ()
  "goto-next: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 16)
    (should (string= "t t" (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1255-next-no-done ()
  "goto-next: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 22)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1260-next-no-done ()
  "goto-next: no DONE, two tasks, point at second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 23)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1270-next-no-done ()
  "goto-next: no DONE, two tasks, point just inside second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 24)
    (should (string= "- s" (bytes-at (point) 3)))
    (should (= 9 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1280-next-no-done ()
  "goto-next: no DONE, two tasks, point two bytes into second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 25)
    (should (string= " se" (bytes-at (point) 3)))
    (should (= 9 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1290-next-no-done ()
  "goto-next: no DONE, two tasks, point near end of second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char (point-max))
    (should (string= "sk\n" (bytes-at (point) 3)))
    (should (= 23 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1310-next-w-done ()
  "goto-next: with DONE, four tasks, point at start of buffer"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 1)
    (should (string= "   " (bytes-at (point) 3)))
    (should (= 8 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1320-next-w-done ()
  "goto-next: with DONE, four tasks, point just before 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 7)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 8 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1330-next-w-done ()
  "goto-next: with DONE, four tasks, point at 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 8)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1340-next-w-done ()
  "goto-next: with DONE, four tasks, point one byte into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 9)
    (should (string= "- t" (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1350-next-w-done ()
  "goto-next: with DONE, four tasks, point two bytes into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 10)
    (should (string= " ta" (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1360-next-w-done ()
  "goto-next:  with DONE, four tasks, point in middle of 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 14)
    (should (string= "k o" (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1370-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 20)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 21 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1380-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 21)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1390-next-w-done ()
  "goto-next:  with DONE, four tasks, point a couple bytes into 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 24)
    (should (string= "tas" (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1392-next-w-done ()
  "goto-next:  with DONE, four tasks, point just before 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 33)
    (should (string= " \n\n" (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1394-next-w-done ()
  "goto-next:  with DONE, four tasks, point at 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 35)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 36 (do-goto-next-task)))
    (should (string= " + " (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1396-next-w-done ()
  "goto-next:  with DONE, four tasks, point at end of 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 48)
    (should (string= "e\n\n" (bytes-at (point) 3)))
    (should (= 106 (do-goto-next-task)))
    (should (string= " + " (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1398-next-w-done ()
  "goto-next:  with DONE, four tasks, point in 4th (completed) task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 107)
    (should (string= "+ f" (bytes-at (point) 3)))
    (should (= 121 (do-goto-next-task)))
    (should (string= " + " (bytes-at 106 3)))
    (should (string= " + " (bytes-at 121 3)))))

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
    (should (string= "ne 2\n" (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 121 result))
    (should (string= " + also" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1405-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 124)
    (should (string= "also " (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 121 result))
    (should (string= " + also" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1410-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 123)
    (should (string= " also " (bytes-at (point) 6)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1420-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in last task by 1 byte"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 122)
    (should (string= "+ also" (bytes-at (point) 6)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1430-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point at last task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 121)
    (should (= 121 (point)))
    (should (string= " + al" (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1440-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 110)
    (should (string= "inish" (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1442-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point early in penultimate task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 109)
    (should (string= "finis" (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1444-prev-w-done ()
  "goto-prev: with DONE, 5 tasks, point in penultimate task mark"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 108)
    (should (string= " fini" (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 36 result))
    (should (string= " - task thr" (bytes-at result 11)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1450-prev-w-done ()
  "prev task: with DONE, 5 tasks, point before 1st task, land on 1st"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 5)
    (should (string= "\n\n\n -" (bytes-at (point) 5)))
    (setq result (do-goto-prev-task))
    (should (= 8 result))
    (should (string= " - tas" (bytes-at result 6)))))

;; ============================================================================
;; tests for do-new-entry
;;

;; (ert-deftest test-1600-new-001 ()
;;   "new entry: empty file"
;;   (with-temp-buffer
;;     (do-new-entry)
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-002 ()
;;   "new entry: no DONE line, one task, before 1st"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-003 ()
;;   "new entry: no DONE line, one task, after 1st"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-004 ()
;;   "new entry: no DONE line, two tasks, before 1st"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-005 ()
;;   "new entry: no DONE line, two tasks, before 2nd"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-006 ()
;;   "new entry: no DONE line, two tasks, after 2nd"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-007 ()
;;   "new entry: no DONE line, three tasks, before 1st"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-008 ()
;;   "new entry: no DONE line, three tasks, before 2nd"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-009 ()
;;   "new entry: no DONE line, three tasks, before 3rd"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-010 ()
;;   "new entry: no DONE line, three tasks, after 3rd"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-011 ()
;;   "new entry: DONE line present, no tasks"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-012 ()
;;   "new entry: DONE line present, one task, before 1st"
;;   (with-temp-buffer
;;     (should nil)))
;; 
;; (ert-deftest test-1600-new-012 ()
;;   "new entry: DONE line present, one task, after 1st"
;;   (with-temp-buffer
;;     (should nil)))

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
    (should (string= " - first" (bytes-at 9 8)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1703-ntm-no-done ()
  "next-task: no DONE, two tasks, point just before first task"
  (with-temp-buffer
    (let ((result))
      (insert buf-no-done)
      (goto-char 7)
      (should (string= "\n\n " (bytes-at (point) 3)))
      (setq result (do-next-task-mark))
      (should (= 9 result))
      (should (string= " - first" (bytes-at result 8))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1706-ntm-no-done ()
  "next-task: no DONE, two tasks, point at first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 9)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1709-ntm-no-done ()
  "next-task: no DONE, two tasks, point just inside first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 10)
    (should (string= "- f" (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1712-ntm-no-done ()
  "next-task: no DONE, two tasks, point two bytes into first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 11)
    (should (string= " fi" (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1715-ntm-no-done ()
  "next-task: no DONE, two tasks, point in middle of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 16)
    (should (string= "t t" (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1718-ntm-no-done ()
  "next-task: no DONE, two tasks, point at end of first task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 22)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1721-ntm-no-done ()
  "next-task: no DONE, two tasks, point at second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 23)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1724-ntm-no-done ()
  "next-task: no DONE, two tasks, point just inside second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 24)
    (should (string= "- s" (bytes-at (point) 3)))
    (should (= 9 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1727-ntm-no-done ()
  "next-task: no DONE, two tasks, point two bytes into second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char 25)
    (should (string= " se" (bytes-at (point) 3)))
    (should (= 9 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1730-ntm-no-done ()
  "next-task: no DONE, two tasks, point near end of second task"
  (with-temp-buffer
    (insert buf-no-done)
    (goto-char (point-max))
    (should (string= "sk\n" (bytes-at (point) 3)))
    (should (= 23 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1733-ntm-w-done ()
  "next-task: with DONE, four tasks, point at start of buffer"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 1)
    (should (string= "   " (bytes-at (point) 3)))
    (should (= 8 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1736-ntm-w-done ()
  "next-task: with DONE, four tasks, point just before 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 7)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 8 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1739-ntm-w-done ()
  "next-task: with DONE, four tasks, point at 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 8)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1742-ntm-w-done ()
  "next-task: with DONE, four tasks, point one byte into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 9)
    (should (string= "- t" (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1745-ntm-w-done ()
  "next-task: with DONE, four tasks, point two bytes into 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 10)
    (should (string= " ta" (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1748-ntm-w-done ()
  "next-task:  with DONE, four tasks, point in middle of 1st task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 14)
    (should (string= "k o" (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1751-ntm-w-done ()
  "next-task:  with DONE, four tasks, point just before 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 20)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 21 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1754-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 21)
    (should (string= " - " (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1757-ntm-w-done ()
  "next-task:  with DONE, four tasks, point a couple bytes into 2nd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 24)
    (should (string= "tas" (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1760-ntm-w-done ()
  "next-task:  with DONE, four tasks, point just before 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 33)
    (should (string= " \n\n" (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1763-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 35)
    (should (string= "\n -" (bytes-at (point) 3)))
    (should (= 36 (do-next-task-mark)))
    (should (string= " + " (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1766-ntm-w-done ()
  "next-task:  with DONE, four tasks, point at end of 3rd task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 48)
    (should (string= "e\n\n" (bytes-at (point) 3)))
    (should (= 106 (do-next-task-mark)))
    (should (string= " + " (bytes-at 106 3)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1769-ntm-w-done ()
  "next-task:  with DONE, four tasks, point in 4th (completed) task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 107)
    (should (string= "+ f" (bytes-at (point) 3)))
    (should (= 121 (do-next-task-mark)))
    (should (string= " + " (bytes-at 106 3)))
    (should (string= " + " (bytes-at 121 3)))))

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
      (should (string= "ne 2\n" (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 121 result))
      (should (string= " + also" (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1805-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at eobp"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 124)
      (should (string= "also " (bytes-at (point) 5)))
      (setq result (do-prev-task-mark))
      (should (= 121 result))
      (should (string= " + also" (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1810-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in last task"
  (with-temp-buffer
    (let ((result))
      (insert buf-w-done)
      (goto-char 123)
      (should (string= " also " (bytes-at (point) 6)))
      (setq result (do-prev-task-mark))
      (should (= 106 result))
      (should (string= " + fini" (bytes-at result 7))))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1815-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in last task by 1 byte"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 122)
    (should (string= "+ also" (bytes-at (point) 6)))
    (setq result (do-prev-task-mark))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1820-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point at last task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 121)
    (should (= 121 (point)))
    (should (string= " + al" (bytes-at (point) 5)))
    (setq result (do-prev-task-mark))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1825-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in penultimate task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 110)
    (should (string= "inish" (bytes-at (point) 5)))
    (setq result (do-prev-task-mark))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1830-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point early in penultimate task"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 109)
    (should (string= "finis" (bytes-at (point) 5)))
    (setq result (do-prev-task-mark))
    (should (= 106 result))
    (should (string= " + fini" (bytes-at result 7)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1835-ptm-w-done ()
  "prev-task: with DONE, 5 tasks, point in penultimate task mark"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 108)
    (should (string= " fini" (bytes-at (point) 5)))
    (setq result (do-prev-task-mark))
    (should (= 36 result))
    (should (string= " - task thr" (bytes-at result 11)))))

;; ----------------------------------------------------------------------------
(ert-deftest test-1840-ptm-w-done ()
  "prev task: with DONE, 5 tasks, point before 1st task, land on 1st"
  (with-temp-buffer
    (insert buf-w-done)
    (goto-char 5)
    (should (string= "\n\n\n -" (bytes-at (point) 5)))
    (setq result (do-prev-task-mark))
    (should (= 8 result))
    (should (string= " - tas" (bytes-at result 6)))))

