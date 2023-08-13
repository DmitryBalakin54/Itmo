package expression.generic.expressionoperations.standartoperations;

import expression.generic.expressionoperations.ExpressionObject;
import expression.generic.expressionoperations.MultiExpression;

public class Const<T> extends ExpressionObject<T> implements MultiExpression<T> {

    private final T value;
    public Const(T value) {
        this.value = value;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @Override
    public boolean equals(Object expression) {
        if (expression == null || this.getClass() != expression.getClass()) {
            return false;
        }

        return value == ((Const<?>) expression).value;
    }
}
