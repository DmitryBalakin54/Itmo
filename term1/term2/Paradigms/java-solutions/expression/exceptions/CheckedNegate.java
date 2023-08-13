package expression.exceptions;

import expression.MultiExpression;
import expression.Negate;

public class CheckedNegate extends Negate {
    public CheckedNegate(MultiExpression value) {
        super(value);
    }

    @Override
    public int makeOperation(int x) {
        if (x == Integer.MIN_VALUE) {
            throw new OverflowException(toString());
        }
        return super.makeOperation(x);
    }
}
