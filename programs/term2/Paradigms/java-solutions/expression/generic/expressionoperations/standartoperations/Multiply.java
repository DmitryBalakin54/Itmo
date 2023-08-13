package expression.generic.expressionoperations.standartoperations;

import expression.generic.expressionoperations.MultiExpression;
import expression.generic.expressionoperations.Operation;
import expression.generic.operations.GenericOperations;

public class Multiply<T> extends Operation<T> implements MultiExpression<T> {

    public Multiply(MultiExpression<T> leftOperand, MultiExpression<T> rightOperand, GenericOperations<T> operation) {
        super(leftOperand, rightOperand, operation);
    }

    @Override
    public T makeOperation(T left, T right) {
        return operation.multiply(left, right);
    }

    @Override
    protected String setSign() {
        return "*";
    }
}
