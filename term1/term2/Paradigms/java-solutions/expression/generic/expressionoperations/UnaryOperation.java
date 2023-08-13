package expression.generic.expressionoperations;

import expression.generic.operations.GenericOperations;

public abstract class UnaryOperation<T> extends ExpressionObject<T> implements MultiExpression<T> {
    protected final MultiExpression<T> operand;
    protected final GenericOperations<T> operation;

    public UnaryOperation(MultiExpression<T> operand, GenericOperations<T> operation) {
        this.operand = operand;
        this.operation = operation;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return makeOperation(operand.evaluate(x, y, z));
    }

    public T makeOperation(T operand) {
        return null;
    }

}
