package expression.exceptions;

import expression.*;

public class CheckedAdd extends Add {
    public CheckedAdd(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }

    public int makeOperation(int left, int right) {
        if (left > 0 && Integer.MAX_VALUE - left < right || right < 0 && Integer.MIN_VALUE - right > left ) {
            throw new OverflowException(toString());
        }
        return super.makeOperation(left, right);
    }
}
