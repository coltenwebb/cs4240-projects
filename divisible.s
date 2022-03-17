.text
.globl main
.globl puti
.globl putc
.globl getc
.globl geti
puti:
    lw $a0, 0($sp)
    li, $v0, 1
    syscall
    jr $ra
putc:
    lw $a0, 0($sp)
    li, $v0, 11
    syscall
    jr $ra
getc:
    li, $v0, 12
    syscall
    add $v0, $v0, $zero
    jr $ra
geti:
    li, $v0, 5
    syscall
    add $v0, $v0, $zero
    jr $ra
divisible:
    addi $sp, $sp, -4
    lw $t8, -4($fp)
    lw $t9, -8($fp)
    div $t8, $t9
    mflo $t8
    sw $t8, -12($fp)
    lw $t8, -12($fp)
    lw $t9, -8($fp)
    mult $t8, $t9
    mflo $t8
    sw $t8, -12($fp)
    lw $t8, -4($fp)
    lw $t9, -12($fp)
    bne $t8, $t9, divisible_label0
    li, $v0, 1
    jr $ra
divisible_label0:
    li, $v0, 0
    jr $ra
main:
    add $fp, $sp, $zero
    addi $sp, $sp, -12
    addi $t8, $zero, 4
    sw $t8, -4($fp)
    addi $t8, $zero, 2
    sw $t8, -8($fp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    lw $t8, -4($fp)
    sw $t8, -4($sp)
    lw $t8, -8($fp)
    sw $t8, -8($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -8
    jal main_divisible
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $v0, -12($fp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    lw $t8, -12($fp)
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal puti
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 10
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 112
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 111
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 103
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 103
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 101
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 114
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 115
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)
    sw $t0, -4($sp)
    sw $t1, -8($sp)
    sw $t2, -12($sp)
    sw $t3, -16($sp)
    sw $t4, -20($sp)
    sw $t5, -24($sp)
    sw $t6, -28($sp)
    sw $t7, -32($sp)
    sw $ra, -36($sp)
    sw $fp, -40($sp)
    addi $sp, $sp, -40
    li, $t8, 10
    sw $t8, -4($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -4
    jal putc
    add $sp, $fp, $zero
    addi $sp, $sp, 40
    lw $fp, -40($sp)
    lw $ra, -36($sp)
    lw $t7, -32($sp)
    lw $t6, -28($sp)
    lw $t5, -24($sp)
    lw $t4, -20($sp)
    lw $t3, -16($sp)
    lw $t2, -12($sp)
    lw $t1, -8($sp)
    lw $t0, -4($sp)