package expression.generic.expressionoperations;

import expression.exceptions.CalculationException;
import expression.generic.operations.GenericOperations;

public abstract class Operation<T> extends ExpressionObject<T> implements MultiExpression<T> {

    protected final MultiExpression<T> leftOperand;
    protected final MultiExpression<T> rightOperand;
    protected final GenericOperations<T> operation;

    protected Operation(MultiExpression<T> leftOperand, MultiExpression<T> rightOperand, GenericOperations<T> operation) {
        this.leftOperand = leftOperand;
        this.rightOperand = rightOperand;
        this.operation = operation;
    }

    public T makeOperation(T left, T right) throws CalculationException {
        return null;
    }

    @Override
    public String toString() {
        return "(" + leftOperand.toString() + " " + setSign() + " " + rightOperand.toString() + ")";
    }

    protected String setSign() {
        return null;
    }


    @Override
    public T evaluate(T x, T y, T z) throws CalculationException {
        return makeOperation(leftOperand.evaluate(x, y, z), rightOperand.evaluate(x, y, z));
    }

    @Override
    public boolean equals(Object expression) {
        if (expression == null || expression.getClass() != this.getClass()) {
            return false;
        }

        return leftOperand.equals(((Operation<?>) expression).leftOperand) && rightOperand.equals(((Operation<?>) expression).rightOperand);
    }

}
