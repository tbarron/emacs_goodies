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
import re
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
    counts = [{'name': '!@!',
               'rgx': "!@!",
               'count': 0},
              {'name': 'tests',
               'rgx': "ert-deftest",
               'count': 0},
              ]

    for path in collect_files_r("."):
        count_lines(path, counts)
    rval = ""
    for nub in counts:
        rval += "{:>15s}: {:10d}\n".format(nub['name'], nub['count'])
    return rval


# -----------------------------------------------------------------------------
def count_lines(path, counts):
    """
    scan file *path* and count up '!@!' lines and 'ert-deftest' lines
    """
    with open(path, 'r') as rbl:
        text = rbl.read()
    for nub in counts:
        nub['count'] = sum(1 for _ in text.split("\n")
                           if re.search(nub['rgx'], _))


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
