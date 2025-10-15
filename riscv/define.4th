
MODULE: RISCV_MOD


: SaveReturnAddress 
    S" addi sp, sp, -16" EVALUATE  \ allocate 16 bytes on stack
    S" sw   ra, 12(sp)" EVALUATE  \ store return address on stack
;

: RestoreReturnAddress
    S" lw   ra, 12(sp)" EVALUATE  \ load return address from stack
    S" addi sp, sp, 16" EVALUATE  \ restore stack pointer
;

: end_word
    S" addi s0, s0, CELL_SIZE" EVALUATE
    S" lw t0, 0(s0)" EVALUATE
    S" jalr ra, t0, 0" EVALUATE
;


;MODULE

MODULE: TCCM

#define UART_BASE 0x10000000
#define RAM_END 0x88000000

;MODULE
