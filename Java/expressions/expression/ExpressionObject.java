package expression;

public abstract class ExpressionObject implements MultiExpression {

    @Override
    public int hashCode() {
        return toString().hashCode();
    }

}
