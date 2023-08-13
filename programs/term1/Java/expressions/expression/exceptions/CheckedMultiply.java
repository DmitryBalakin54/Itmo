package expression.exceptions;

import expression.MultiExpression;
import expression.Multiply;

public class CheckedMultiply extends Multiply {
    public CheckedMultiply(MultiExpression left, MultiExpression right) {
        super(left, right);
    }

    public int makeOperation(int left, int right) {
        if (left == Integer.MIN_VALUE && right == -1 ||
                right == Integer.MIN_VALUE && left == -1 ||
                left > 0 && right > 0 && left > Integer.MAX_VALUE / right ||
                left < 0 && right < 0 && left < Integer.MAX_VALUE / right ||
                left > 0 && right < 0 && right < Integer.MIN_VALUE / left ||
                left < 0 && right > 0 && left < Integer.MIN_VALUE / right) {
            throw new OverflowException(toString());
        }
        return super.makeOperation(left, right);
    }
}
