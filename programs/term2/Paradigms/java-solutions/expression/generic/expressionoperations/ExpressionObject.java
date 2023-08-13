package expression.generic.expressionoperations;

public abstract class ExpressionObject<T> implements MultiExpression<T> {

    @Override
    public int hashCode() {
        return toString().hashCode();
    }

}
