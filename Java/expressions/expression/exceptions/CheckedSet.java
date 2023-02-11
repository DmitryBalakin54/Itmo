package expression.exceptions;

import expression.MultiExpression;
import expression.Set;

public class CheckedSet extends Set {
    public CheckedSet(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }


    public int makeOperation(int left, int right) {
        return super.makeOperation(left, right);
    }
}
