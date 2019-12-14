## 1.0.2 ... 2019-12-14 17:19:48

 * Settled on a set of key commands for do-mode
   (6ed52fb2)
 * Use do-{next,prev}-task-mark in do-goto-{next,prev}-task for less repetition
   and improved simplicity
   (6a000e2e3)
 * Figured out running tests with ert and saved this intelligence in Makefile
   (169f172)
 * Determined that with-output-to-string does not suppress the output of
   (message ...) calls to stdout during tests
   (2a642e55)
 * Invented qrepl -- a search and replace function where the replacement text
   changes with each replacement. This is good for changes where the target
   should be an ascending sequence of numbers, for example.
   (f06d2a2)
 * Completed tests and payload for:
    > do-done                         (ccb3bd0)
    > do-[pxo]done                    (c3896f9)
    > do-add-done-iff                 (53115f9)
    > do-new-task                     (a91316f)
    > {previous,next}-dodo            (9605ab1)
    > do-done-position                (53115f9)
    > do-goto-{next,prev}-task        (53115f9)
    > do-{next,prev}-task-mark        (53115f9)
    > do-task-up                      (3919794)
    > do-task-down                    (da9fbc5)
    > bytes-at                        (681bfa7)
    > do-buffer-p                     (a6658c2)
    > next-dodo, previous-dodo        (2f02fc7)
 * Expanded test framework:
    > Wrote emacs-test.py for running emacs tests a la py.test
    > Added interactive-tests.el for running tests interactively in emacs
    > Move test-do-mode.el into tests directory

## 1.0.1 ... 2019-11-29 21:30:42

 * New functions:
    > do-done (mark a task and move it down in the DONE section),
    > do-pdone (calls do-done with " + " to indicate a completed task),
    > do-xdone (calls do-done with " x " to indicate an abandoned task),
    > do-odone (calls do-done with " < " to indicate a diverted task),
    > next-dodo (finds next DODO file in buffer list),
    > previous-dodo (finds previous DODO file in buffer list)
 * Assign key seqs for the new functions as needed
 * Report when do-mode.el gets loaded
 * Set fill-column to 80 for most purposes
 * Add filenames matching "do$" to the do-mode auto mode list
 * .gitignore a test directory where we can put untracked test files

## 1.0.0 ... 2019-11-24 15:31:11

 * Make diary (\M-j, \C-xj) and mydoc (\C-xm) work as desired.
 * Assign version 1.0.0 with file version.el and load it at startup (\M-v).
 * Make emacs_goodies part of the community of projects in ~/prj/github
 * This repository has a long history and we're just adding CHANGELOG now.
   Oh, well.
