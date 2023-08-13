package expression.exceptions;

import expression.Divide;
import expression.MultiExpression;

public class CheckedDivide extends Divide {
    public CheckedDivide(MultiExpression left, MultiExpression right) {
        super(left, right);
    }

    public int makeOperation(int left, int right) {
        if (right == 0) {
            throw new DivisionByZeroException(toString());
        }
        if (left == Integer.MIN_VALUE && right == -1) {
            throw new OverflowException(toString());
        }
        return super.makeOperation(left, right);
    }
}
