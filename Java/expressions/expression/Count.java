package expression;

public class Count extends ExpressionObject implements MultiExpression {

    protected final MultiExpression operand;

    public Count(MultiExpression operand) {
        this.operand = operand;
    }

    public int makeOperation(int x) {
        String str = Integer.toBinaryString(x);
        int counter = 0;
        for (char ch : str.toCharArray()) {
            counter += ch == '1' ? 1 : 0;
        }
        return counter;
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
        return "count(" + operand.toString() + ")";
    }

}
