#include "textflag.h"

#define ZERO(r) \
    XORQ r, r

TEXT ·Sum(SB), NOSPLIT, $0-24
    MOVQ x_data+0(FP), AX
    MOVQ x_len+8(FP), DX
    ZERO(R10)

    TESTQ DX, DX
    JZ done

loop:
    MOVLQSX (AX), R9
    ADDQ R9, R10
    ADDQ $4, AX
    DECQ DX
    JNZ loop

done:
    MOVQ R10, ret+24(FP)
    RET
