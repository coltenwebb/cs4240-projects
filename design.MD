# Design Decisions

## Physical Registers Used

- $t0-$t9 Temporary Registers
  - $t0-$t6 General use
  - $t7-$t9 Reserved for spillover usage
- $v0 Return Value
- $sp, $fp for calling convention
- $31 Return address

## Calling Convention
```
       ---------------------

$sp'     spillover regs        callee responsible for moving sp up for spillover reg
      
       ---------------------
          ...

      ---------------------
$sp ->
       -------------------
          arg n (for callee)
      -------------------
          arg n - 1
       -------------------
              ...
       -------------------
            arg 0
       ------------------  static in size  across every call 
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
         args n (for caller)



old fp ------------------------------
```

Caller responsible for both setup and teardown of stack.
Including moving the sp and fp back to original positions.

### Caller Stack Setup
* Stack grows from high to low address, meaning we decrement `$sp` to push to stack.
```
$sp = $sp - 1  

for (i in range(0,9)):
   MEM[$sp - i] = $t_i

MEM[$sp - 10] = $ra

MEM[$sp - 11] = $fp

$fp = $sp

$sp = $sp - 11

for (i in range(len(args))):
   $sp = $sp - 1
   MEM[$sp] = args[i]

# jump using jal
# "jal label" 
#  - $ra <- pc
#  - $pc <- label 

jal callee_label
```

### Caller Stack Teardown
```
$sp = $fp
$fp = MEM[$sp - 11]
$ra = MEM[$sp - 10]
for (i in range(0,9)):
   $t_i = MEM[$sp + i]

# pop stack by 1 since we pushed by one in the 
# beginning of the stack setup
$sp = $sp + 1
```

<!-- sp = fp
fp = mem_in_stack[$old_fp]

for i in range(0, 9)
  LW $t{i} $sp+whatever+i

LW $return addr $sp+whatever -->
