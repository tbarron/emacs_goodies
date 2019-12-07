"""
Usage:
    emacs-test go [-d] [-n] [-k WHICH]
    emacs-test collect [-d]
"""
from docopt_dispatch import dispatch
import glob
import os
import pdb
import sys


# -----------------------------------------------------------------------------
@dispatch.on("go")
def collect_and_run(**kw):
    """
    Collect files and run them
    """
    if kw['d']:
        pdb.set_trace()
    which = kw['WHICH'] or 't'
    dryrun = kw['n']
    for path in collect_files_r("."):
        run_test_file(path, which, dryrun)


# -----------------------------------------------------------------------------
@dispatch.on("collect")
def collect(**kw):
    """
    Report which files would be collected and run
    """
    if kw['d']:
        pdb.set_trace()
    flist = collect_files_r(".")
    for item in flist:
        print(item)


# -----------------------------------------------------------------------------
def collect_files_r(path):
    """
    Glob up some files to consider running
    """
    rval = []
    globexp = "{}/test*".format(path)
    for item in glob.glob(globexp):
        if os.path.isdir(item):
            rval.extend(collect_files_r(item))
        else:
            rval.append(item)
    return rval

    
# -----------------------------------------------------------------------------
def run_test_file(path, which, dryrun):
    """
    Run tests matching *which* from file *path*
    """
    lisp_cmd = '(ert-run-tests-batch-and-exit \\"{}\\")'.format(which)
    cmd = 'emacs -batch -l ert -l {} --eval "{}"'.format(path, lisp_cmd)
    print(cmd)
    if not dryrun:
        os.system(cmd)
    

# -----------------------------------------------------------------------------
if __name__ == "__main__":
    dispatch(__doc__)
