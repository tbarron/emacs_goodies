"""
Usage:
    emacs-test go [-d] [-n] [-p] [-k WHICH]
    emacs-test collect [-d]
    emacs-test assess [-d]
"""
from docopt_dispatch import dispatch
import glob
import os
import pdb
import sys


# -----------------------------------------------------------------------------
@dispatch.on("assess")
def assess(**kw):
    """
    Return a count of '!@!' lines vs 'ert-deftest' lines in collected files
    """
    print(assessment())


# -----------------------------------------------------------------------------
def assessment():
    pending_count = test_count = 0
    for path in collect_files_r("."):
        (pending, tests) = count_lines(path)
        pending_count += pending
        test_count += tests
    return("{} / {}".format(pending_count, test_count))


# -----------------------------------------------------------------------------
def count_lines(path):
    """
    scan file *path* and count up '!@!' lines and 'ert-deftest' lines
    """
    with open(path, 'r') as rbl:
        text = rbl.read()
    pending = sum(1 for _ in text.split("\n") if "!@!" in _)
    tests = sum(1 for _ in text.split("\n") if "ert-deftest" in _)
    return(pending, tests)


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
        run_test_file(path, which, dryrun, pager=kw['p'])

    print(assessment())

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
def run_test_file(path, which, dryrun, pager=False):
    """
    Run tests matching *which* from file *path*
    """
    lisp_cmd = '(ert-run-tests-batch-and-exit \\"{}\\")'.format(which)
    cmd = 'emacs -batch -l ert -l {} --eval "{}"'.format(path, lisp_cmd)
    if pager:
        cmd += "| less"
    print(cmd)
    if not dryrun:
        os.system(cmd)


# -----------------------------------------------------------------------------
if __name__ == "__main__":
    dispatch(__doc__)
