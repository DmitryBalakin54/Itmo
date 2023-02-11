package expression;

public class Negate extends ExpressionObject implements MultiExpression {

    protected final MultiExpression operand;

    public Negate(MultiExpression operand) {
        this.operand = operand;
    }

    public int makeOperation(int x) {
        return -x;
    }

    @Override
    public int evaluate(int x) {
        return makeOperation(operand.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return makeOperation(operand.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "-(" + operand.toString() + ")";
    }

}
