package expression.exceptions;

import expression.Count;
import expression.MultiExpression;

public class CheckedCount extends Count {
    public CheckedCount(MultiExpression operand) {
        super(operand);
    }
}
