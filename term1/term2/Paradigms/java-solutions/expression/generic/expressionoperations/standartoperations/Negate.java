package expression.generic.expressionoperations.standartoperations;

import expression.generic.expressionoperations.ExpressionObject;
import expression.generic.expressionoperations.MultiExpression;
import expression.generic.expressionoperations.UnaryOperation;
import expression.generic.operations.GenericOperations;

public class Negate<T> extends UnaryOperation<T> implements MultiExpression<T> {



    public Negate(MultiExpression<T> operand, GenericOperations<T> operation) {
        super(operand, operation);
    }

    public int makeOperation(int x) {
        return -x;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return makeOperation(operand.evaluate(x, y, z));
    }

    @Override
    public T makeOperation(T operand) {
        return operation.negate(operand);
    }

    @Override
    public String toString() {
        return "-(" + operand.toString() + ")";
    }

}
