# Design Decisions

## Physical Registers Used

- $t0-$t9 Temporary Registers
  - $t0-$t6 General use
  - $t7-$t9 Reserved for spillover usage
- $v0 Return Value
- $sp, $fp for calling convention
- $31 Return address

## Calling Convention

     ---------------------
$sp ->
       -------------------
          arg n
       -------------------
          arg n - 1
       -------------------
              ...
       -------------------
            arg 0
static in size  across every call ------------------
          $old_fp
       -------------------
          $t31 (return addr)
       -------------------
          $t9
       -------------------
          ...
       -------------------
fp/old sp ->     $t0 
       -------------------




old fp ------------------------------


Caller responsible for both setup and teardown of stack.
Including moving the sp and fp back to original positions.

teardown

sp = fp
fp = mem_in_stack[$old_fp]

for i in range(0, 9)
  LW $t{i} $sp+whatever+i

LW $return addr $sp+whatever
