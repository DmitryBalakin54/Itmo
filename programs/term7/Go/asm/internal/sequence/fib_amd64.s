#include "textflag.h"

#define ZERO(r) \
    XORQ r, r


TEXT ·Fibonacci(SB), NOSPLIT, $0-8
    MOVQ n+0(FP), CX

    TESTQ CX, CX
    JE isZero

    CMPQ CX, $2
    JBE oneOrTwo

    ZERO(R8)
    MOVQ $1, R9
    MOVQ R9, R10
    MOVQ R9, R11

    MOVQ R9, R12
    MOVQ R8, R13
    MOVQ R8, R14
    MOVQ R9, R15

    SUBQ $1, CX
loop:
    TESTQ CX, CX
    JE done

    TESTQ $1, CX
    JZ even

    MOVQ R12, AX
    MULQ R8
    MOVQ AX, BP
    MOVQ R13, AX
    MULQ R10
    ADDQ AX, BP

    MOVQ R12, AX
    MULQ R9
    MOVQ AX, BX
    MOVQ R13, AX
    MULQ R11
    ADDQ AX, BX

    MOVQ R14, AX
    MULQ R8
    MOVQ AX, DI
    MOVQ R15, AX
    MULQ R10
    ADDQ AX, DI

    MOVQ R14, AX
    MULQ R9
    MOVQ AX, SI
    MOVQ R15, AX
    MULQ R11
    ADDQ AX, SI

    MOVQ BP, R12
    MOVQ BX, R13
    MOVQ DI, R14
    MOVQ SI, R15

even:
    MOVQ R8, SI
    ADDQ R11, SI

    MOVQ R9, AX
    MULQ R10
    MOVQ AX, DI

    MOVQ R8, AX
    MULQ AX
    MOVQ AX, R8
    ADDQ DI, R8

    MOVQ R11, AX
    MULQ AX
    MOVQ AX, R11
    ADDQ DI, R11

    MOVQ R9, AX
    MULQ SI
    MOVQ AX, R9

    MOVQ R10, AX
    MULQ SI
    MOVQ AX, R10

    SHRQ $1, CX
    JMP loop

done:
    MOVQ R15, ret+8(FP)
    RET

isZero:
    MOVQ $0, ret+8(FP)
    RET

oneOrTwo:
    MOVQ $1, ret+8(FP)
    RET
