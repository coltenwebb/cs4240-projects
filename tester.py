#!/usr/bin/env python3

import time
import argparse
from pathlib import Path
from subprocess import Popen, PIPE, TimeoutExpired
import re
import tempfile

# we want to run optimized and baseline against an input at the same time,
# then to print the diff
# the diff should just be number of instruction executed

# these should be commands to execute
# %s gets replaced with the ir filepath
OPTIMIZER = 'cabal run -v0 project1-app %s'
INTERPRETER = 'java -cp ../example-project1/materials/src/ IRInterpreter %s'

def main(args):
    if args.ir.is_dir():
        go_on_dir(args)
    else:
        go(args.ir, args)

def exec_cmd(cmd, inp=b"", f=None):
    """
    does the command and returns its output.
    if a file is passed writes to that file.
    """
    if not f:
        f = tempfile.NamedTemporaryFile().name
    fw = open(f, "wb")
    fr = open(f, "r")
    p = Popen(cmd, shell=True,stdin = PIPE, stdout = fw, stderr = fw)
    p.stdin.write(inp)
    p.stdin.flush()
    try:
        p.wait(timeout=5)
    except TimeoutExpired:
        raise
    finally:
        p.kill()
    output = fr.read() # reads file into ordinary string (not bytestring)
    fw.close()
    fr.close()
    return output


def go_on_dir(args):
    files = list(args.ir.iterdir())
    files.sort()
    for irpath in files:
        if ".DS_Store" in str(irpath):
            continue
        print(str(irpath.name) + ": ", end='')
        if not irpath.is_file():
            continue
        try:
            code = go(irpath,args)  
        except Exception as err:
            if 'Interpreter failed on baseline' in str(err):
                # bad IR, so let's just remove it and continue
                irpath.unlink()
            else:
                raise


def go(irpath, args):
    # so let's get both irs that we want


    inp = b""
    if args.input is not None:
        with open(args.input, "rb") as inpf:
            inp = inpf.read()

    # first, interpret the baseline ir
    try:
        ogout = exec_cmd(INTERPRETER %str(irpath),inp)
    except TimeoutExpired:
        raise Exception('Interpreter failed on baseline, because it timed out')

    if "Number of non-label instructions executed" not in ogout:
        print("INTERPRETER FAILED ON BASELINE WITH OUTPUT:")
        print(ogout)
        raise Exception('Interpreter failed on baseline, likely because you need to change the INTERPRETER path at the top of script, or because the IR you provided is invalid')


    # next, run the ir thru the optimizer
    irtmp = tempfile.NamedTemporaryFile() # where we write the optimized ir to
    out = exec_cmd(OPTIMIZER % str(irpath), f=irtmp.name)
    if "#start_function" not in out:
        print("OPTIMIZER FAILED WITH OUTPUT:")
        print(out)
        if args.verbose:
            print("BASELINE IR:")
            with open(irpath, "r") as f:
                print(f.read())
        raise Exception('Optimizer failed to produce valid IR, likely because you need to change the OPTIMIZER path at the top of script, or because it crashed')

    # interpret the optimized ir
    optimout = exec_cmd(INTERPRETER % irtmp.name,inp)
    if "Number of non-label instructions executed" not in optimout:
        print("INTERPRETER FAILED ON OPTIMIZED IR WITH OUTPUT:")
        print(optimout)
        if args.verbose:
            print("BASELINE IR:")
            with open(irpath, "r") as f:
                print(f.read())
            print("OPTIMIZED IR:")
            print(out)
        raise Exception('Interpreter failed on optimized output, likely because the optimizer produced invalid IR')

    try:
        # the last line of the interpreter should look like:
        # Number of non-label instructions executed: 434

        # we compare the outputs of the programs by removing this line,
        # and we print the change in instruction count

        pat = re.compile(r"Number of non-label instructions executed: ([0-9]+)")
        
        matchbaseline = pat.search(ogout)
        baselineprogout = ogout[:matchbaseline.start()]
        baselinecount = int(matchbaseline[1])

        matchoptim = pat.search(optimout)
        optimprogout = optimout[:matchoptim.start()]
        optimcount = int(matchoptim[1])

        # first order of business: ensure exact same outputs
    except Exception:
        if args.verbose:
            print("OUTPUT OF BASELINE PROGRAM")
            print(ogout)
            print("OUTPUT OF OPTIMIZED PROGRAM")
            print(optimout)
        raise Exception("Couldn't parse the outputs of the interpreter. Are you using the example java interpreter?")

    if optimprogout != baselineprogout:
        print("OUTPUT OF BASELINE PROGRAM")
        print(ogout)
        print("OUTPUT OF OPTIMIZED PROGRAM")
        print(optimout)
        raise Exception("Outputs of baseline and optimized ir are different.")

    if args.verbose:
        print("OPTIMIZED IR:")
        print(out)
        print("OUTPUT OF BASELINE PROGRAM")
        print(ogout)
        print("OUTPUT OF OPTIMIZED PROGRAM")
        print(optimout)

    if baselinecount >= optimcount:
        print(f"Outputs match, DIC {str(baselinecount)} -> {str(optimcount)}")
    else:
        print(f"!!! Dynamic instruction count increased from {str(baselinecount)} to {str(optimcount)}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='compare optimized ir dynamic instruction count against baseline using interpreter. also compares outputs.')
    parser.add_argument('ir', type=Path, help='the ir to optimize and run. runs on all files if you pass a directory.')
    parser.add_argument('-i','--input', type=Path, help='the input to feed into the programs')
    parser.add_argument('-v', '--verbose', action='store_true', help='whether to print optimized IR and the outputs of the programs')
    args = parser.parse_args()
    main(args)
