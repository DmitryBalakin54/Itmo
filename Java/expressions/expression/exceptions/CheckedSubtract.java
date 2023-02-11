package expression.exceptions;

import expression.MultiExpression;
import expression.Subtract;

public class CheckedSubtract extends Subtract {
    public CheckedSubtract(MultiExpression left, MultiExpression right) {
        super(left, right);
    }

    public int makeOperation(int left, int right) {
        if (right < 0 &&  left > Integer.MAX_VALUE + right || right > 0 && left  < Integer.MIN_VALUE + right) {
            throw new OverflowException(toString());
        }
        return super.makeOperation(left, right);
    }
}
