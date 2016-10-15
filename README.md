# My collection of emacs support files

## Actively in use
  * tmp.el
    * Place to experiment with new ideas. Not loaded automatically.
  * diary.el
    * This is mostly obsolete but I still use some of the date insert
      functions. Loaded by .emacs.
  * do-mode.el
    * Not using emacs for todo lists so much anymore, but this has not quite gone 
      completely away. Loaded by .emacs.
  * html.el
    * HTML mode editing programs. Loaded by .emacs.
  * *mykeys.el*: Personal key bindings. Over time I want to pull most of
    my key bindings out of .emacs and put them here. Loaded by .emacs.
  * foo.el
    * Routine to sort a paren-enclosed list. Not loaded by .emacs.


## Sorting
  * org-journal.el
    * Loaded by .emacs.
  * python-mode.el
    * Loaded by .emacs.
  * sccs.el
    * Loaded by .emacs.
  * status.el
    * Loaded by .emacs.
  * support.el
    * Loaded by .emacs.
  * tools.el
    * Loaded by .emacs.
  * unix.el
    * Loaded by .emacs.
  * wc-mode.el
    * Not loaded by .emacs.
  * word-count.el

## Obsolete
  * mail-aliases.el
    * Old mail aliases for when I was reading mail in emacs. Not
      loaded by .emacs.
  * Makefile
    * Target to create tarball for shipping this content somewhere
      else. Now we use git and github for moving this stuff around.

## Notes

To load a file manually, visit the file in the editor and type "\M-v"
(which is bound to the emacs command eval-current-buffer).

"\M-X" means type and release Escape, type and release X.

"\CX-N" means type and release Ctrl-X, type and release N.