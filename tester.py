#!/usr/bin/env python3

import time
import argparse
from pathlib import Path
from subprocess import Popen, PIPE
import re
import tempfile

# we want to run optimized and baseline against an input at the same time,
# then to print the diff
# the diff should just be number of instruction executed

# these should be commands to execute
# program path should be last arg
OPTIMIZER = 'cabal run -v0 project1-app'.split() 
INTERPRETER = 'java -cp ../example-project1/materials/src/ IRInterpreter'.split()

def main(args):
    if args.ir.is_dir():
        go_on_dir(args)
    else:
        go(args.ir, args)

def go_on_dir(args):
    files = args.ir.iterdir()
    for irpath in files:
        if not irpath.is_file():
            continue
        if go(irpath,args) == 1:
            break

def go(irpath, args):
    # so let's get both irs that we want

    irtmp = tempfile.NamedTemporaryFile()
    exectmp = tempfile.NamedTemporaryFile()

    fw = open(irtmp.name, "wb")
    fr = open(irtmp.name, "r")
    p = Popen(OPTIMIZER + [str(irpath)], 
            stdin = PIPE, stdout = fw, stderr = fw)
    p.wait()
    out = fr.read()
    fw.close()
    fr.close()

    if "#start_function" not in out:
        print("OPTIMIZER FAILED:")
        print(out)
        return 1

    if args.verbose:
        print("OPTIMIZED IR:")
        print(out)


    inp = b""
    if args.input is not None:
        with open(args.input, "rb") as inpf:
            inp = inpf.read()

    # now we test against it, first the og/baseline program
    fw = open(exectmp.name, "wb")
    fr = open(exectmp.name, "r")
    p = Popen(INTERPRETER + [str(irpath)], 
            stdin = PIPE, stdout = fw, stderr = fw)
    p.stdin.write(inp)
    p.stdin.flush()
    p.wait()
    ogout = fr.read()
    fw.close()
    fr.close()

    # and against the optimized program
    fw = open(exectmp.name, "wb")
    fr = open(exectmp.name, "r")
    p = Popen(INTERPRETER + [irtmp.name], 
            stdin = PIPE, stdout = fw, stderr = fw)
    p.stdin.write(inp)
    p.stdin.flush()
    p.wait()
    optimout = fr.read()
    fw.close()
    fr.close()

    if optimout is None or ogout is None:
        raise Exception("Couldn't read ir output")


    if args.verbose:
        print("OUTPUT OF BASELINE PROGRAM")
        print(ogout)
        print("OUTPUT OF OPTIMIZED PROGRAM")
        print(optimout)

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
        if optimprogout != baselineprogout:
            raise Exception("Outputs of baseline and optimized ir are different.")
    except Exception:
        print("OPTIMIZED IR")
        print(out)
        print("OUTPUT OF BASELINE PROGRAM")
        print(ogout)
        print("OUTPUT OF OPTIMIZED PROGRAM")
        print(optimout)
        raise

    if baselinecount > optimcount:
        print(f"Dynamic instruction count reduced from {str(baselinecount)} to {str(optimcount)}")
    elif baselinecount == optimcount:
        print(f"Dynamic instruction count stayed at {str(baselinecount)}")
    else:
        print(f"!!! Dynamic instruction count increased from {str(baselinecount)} to {str(optimcount)}")
    return 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='compare optimized ir dynamic instruction count against baseline using interpreter. also compares outputs.')
    parser.add_argument('ir', type=Path, help='the ir to optimize and run. runs on all files if you pass a directory.')
    parser.add_argument('-i','--input', type=Path, help='the input to feed into the programs')
    parser.add_argument('-v', '--verbose', action='store_true', help='whether to print optimized IR and the outputs of the programs')
    args = parser.parse_args()
    main(args)
