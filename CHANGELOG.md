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
