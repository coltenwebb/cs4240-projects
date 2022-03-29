#!/usr/bin/env python3

import sys
import subprocess

testcases = []
target_dir = sys.argv[1]
testcases_file = target_dir + "/testcase_names"
with open(testcases_file, "r") as f:
  for t in f:
    testcases.append(t[:-1])

print("BEGINNING TEST CASES POGGERS!!!")
for tc in testcases:
  tigerir = f"{target_dir}/{tc}.ir"
  expected_out = f"{target_dir}/{tc}.out"
  subprocess.run(["./testcase_helper.sh", tigerir, expected_out])
