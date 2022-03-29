#!/bin/sh

# helper to run one specific test case in run_testcases.py
NAIVE=$(mktemp)
INTRA=$(mktemp)

NAIVE_OUT=$(mktemp)
INTRA_OUT=$(mktemp)

echo "RUNNING: $1" 
# arg $1 = ir file
# arg $2 = expected output
sh run.sh naive $1 $NAIVE 1>/dev/null 2>/dev/null
sh run.sh intra $1 $INTRA 1>/dev/null 2>/dev/null

spim -quiet -f $NAIVE | tail -n 1 > $NAIVE_OUT
spim -quiet -f $INTRA | tail -n 1 > $INTRA_OUT

diff $NAIVE_OUT $2
diff $INTRA_OUT $2