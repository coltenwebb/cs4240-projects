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
    addi $sp, $sp, -56
    addi $t8, $zero, 0
    sw $t8, -8($fp)
    addi $t8, $zero, 2
    sw $t8, -16($fp)
    addi $t8, $zero, 3
    sw $t8, -20($fp)
    addi $t8, $zero, 6
    sw $t8, -24($fp)
    addi $t8, $zero, 0
    sw $t8, -40($fp)
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
    add $fp, $sp, $zero
    addi $sp, $sp, 0
    jal geti
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
    sw $v0, -4($fp)
    lw $t8, -4($fp)
    addi $t9, $zero, 1
    sub $t8, $t8, $t9
    bgtz $t8, main_label0
    addi $t8, $zero, 0
    sw $t8, -12($fp)
    lw $t8, -12($fp)
    sw $t8, -56($fp)
    j  main_print
main_label0: 
    lw $t8, -4($fp)
    addi $t9, $zero, 3
    sub $t8, $t8, $t9
    bgtz $t8, main_label1
    addi $t8, $zero, 1
    sw $t8, -12($fp)
    lw $t8, -12($fp)
    sw $t8, -56($fp)
    j  main_print
main_label1: 
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
    lw $t8, -16($fp)
    sw $t8, -8($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -8
    jal divisible
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
    sw $v0, -28($fp)
    lw $t8, -40($fp)
    sw $t8, -12($fp)
    lw $t8, -12($fp)
    sw $t8, -56($fp)
    lw $t8, -28($fp)
    addi $t9, $zero, 1
    beq $t8, $t9, main_label2
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
    lw $t8, -20($fp)
    sw $t8, -8($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -8
    jal divisible
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
    sw $v0, -28($fp)
    lw $t8, -40($fp)
    sw $t8, -12($fp)
    lw $t8, -12($fp)
    sw $t8, -56($fp)
    lw $t8, -28($fp)
    addi $t9, $zero, 1
    beq $t8, $t9, main_label2
    j  main_label3
main_label2: 
    j  main_print
main_label3: 
    addi $t8, $zero, 5
    sw $t8, -8($fp)
main_loop: 
    lw $t8, -8($fp)
    lw $t9, -8($fp)
    mult $t8, $t9
    mflo $t8
    sw $t8, -32($fp)
    lw $t8, -32($fp)
    lw $t9, -4($fp)
    sub $t8, $t8, $t9
    bgtz $t8, main_exit
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
    jal divisible
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
    sw $v0, -28($fp)
    lw $t8, -40($fp)
    sw $t8, -12($fp)
    addi $t8, $zero, 0
    sw $t8, -44($fp)
    addi $t8, $zero, 0
    sw $t8, -52($fp)
    lw $t8, -12($fp)
    sw $t8, -56($fp)
    lw $t8, -28($fp)
    addi $t9, $zero, 1
    beq $t8, $t9, main_label2
    lw $t8, -8($fp)
    addi $t8, $t8, 2
    sw $t8, -36($fp)
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
    lw $t8, -36($fp)
    sw $t8, -8($sp)
    add $fp, $sp, $zero
    addi $sp, $sp, -8
    jal divisible
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
    sw $v0, -28($fp)
    lw $t8, -40($fp)
    sw $t8, -12($fp)
    lw $t8, -12($fp)
    sw $t8, -56($fp)
    lw $t8, -28($fp)
    addi $t9, $zero, 1
    beq $t8, $t9, main_label2
    lw $t8, -8($fp)
    addi $t8, $t8, 6
    sw $t8, -8($fp)
    j  main_loop
main_exit: 
    lw $t8, -44($fp)
    sw $t8, -48($fp)
    lw $t8, -52($fp)
    sw $t8, -12($fp)
    addi $t8, $zero, 1
    sw $t8, -12($fp)
    lw $t8, -12($fp)
    sw $t8, -56($fp)
main_print: 
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
    lw $t8, -56($fp)
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
    jr $ra