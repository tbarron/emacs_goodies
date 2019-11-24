# My collection of emacs support files
Updated: <2019.1124 15:24:35>

This repository houses my collection of emacs Lisp scripts. 

## Actively in use

#### experimental.el

  * Place to experiment with new ideas. Not loaded automatically.

#### diary.el

  * This is mostly obsolete but I still use some of the functions. Loaded
    by .emacs.

#### do-mode.el

  * Not using emacs for todo lists so much anymore, but this has not quite gone 
    completely away. Loaded by .emacs.

#### html.el

  * HTML mode editing programs. Loaded by .emacs.

#### mykeys.el

  * Personal key bindings. Over time I want to pull most of
    my key bindings out of .emacs and put them here. Loaded by .emacs.

#### org-journal.el

  * An organizational framework I tried out. Not using it anymore
    but it's still loaded by .emacs.

#### python-mode.el

  * Third party python mode which I have customized lightly. Once I
    get py-comment-function working the way I want, it should go in
    here. Loaded by .emacs.

#### foo.el

  * Routine to sort a paren-enclosed list. Not loaded by .emacs.

#### sccs.el

  * Support for ancient version control system. Probably obsolete. Loaded by .emacs.

#### status.el

  * Old code for generating weekly status report. Could be brought
      up to date. Loaded by .emacs.

#### support.el

  * Obsolete mode for editing "support" files from back in the Envoy
    days (default vendor is DG). I think provided a framework for
    tracking support tickets. Could be useful as an example of how to
    implement a major mode. Loaded by .emacs.

#### tools.el

  * General use tools. E.g., bracket a block quote, repeatable forward
    and backwardsearch, double-space the buffer, C function stub, etc.
    Loaded by .emacs.

#### unix.el

  * Unix integration: view a man page, edit a crontab, more could be
    added. Loaded by .emacs.

#### word-count.el

  * Word count mode by Hiroyuki Komatsu. Loaded by .emacs.

## Obsolete

#### wc-mode.el

  * Third party word count mode b Toby Cubitt. Obsolete. Not loaded by
    .emacs.

#### mail-aliases.el

  * Old mail aliases for when I was reading mail in emacs. Not
    loaded by .emacs.

#### Makefile

  * Target to create tarball for shipping this content somewhere
    else. Now we use git and github for moving this stuff around.

## Notes

  * To load a file manually, visit the file in the editor and type "\M-v"
    (which is bound to the emacs command eval-current-buffer).

  * "\M-X" means type and release Escape, type and release X.

  * "\CX-N" means type and release Ctrl-X, type and release N.
