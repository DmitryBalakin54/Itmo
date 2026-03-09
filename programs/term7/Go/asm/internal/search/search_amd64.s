#include "textflag.h"

#define ZERO(r) \
    XORQ r, r

TEXT ·LowerBound(SB), NOSPLIT, $0-32
    MOVQ x_data+0(FP), AX
    MOVQ x_len+8(FP), DX
    MOVQ value+24(FP), CX
    ZERO(R10)

    TESTQ DX, DX
    JZ done

loop:
    CMPQ R10, DX
    JGE done

    MOVQ R10, R8
    ADDQ DX, R8
    SHRQ $1, R8

    MOVQ (AX)(R8*8), R11
    CMPQ R11, CX
    JGE greater

    LEAQ 1(R8), R10
    JMP loop

greater:
    MOVQ R8, DX
    JMP loop

done:
    MOVQ R10, ret+32(FP)
    RET
