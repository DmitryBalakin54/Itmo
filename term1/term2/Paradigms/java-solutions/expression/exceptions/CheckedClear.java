package expression.exceptions;

import expression.MultiExpression;
import expression.Clear;

public class CheckedClear extends Clear {
    public CheckedClear(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }


    public int makeOperation(int left, int right) {
        return super.makeOperation(left, right);
    }
}
