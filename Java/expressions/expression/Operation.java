package expression;

public abstract class Operation extends ExpressionObject implements MultiExpression {

    protected final MultiExpression leftOperand;
    protected final MultiExpression rightOperand;

    protected Operation(MultiExpression leftOperand, MultiExpression rightOperand) {
        this.leftOperand = leftOperand;
        this.rightOperand = rightOperand;
    }

    public int makeOperation(int left, int right) {
        return 0;
    }

    @Override
    public String toString() {
        return "(" + leftOperand.toString() + " " + setSign() + " " + rightOperand.toString() + ")";
    }

    protected String setSign() {
        return null;
    }

    @Override
    public int evaluate(int value) {
        return makeOperation(leftOperand.evaluate(value), rightOperand.evaluate(value));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return makeOperation(leftOperand.evaluate(x, y, z), rightOperand.evaluate(x, y, z));
    }

    @Override
    public boolean equals(Object expression) {
        if (expression == null || expression.getClass() != this.getClass()) {
            return false;
        }

        return leftOperand.equals(((Operation) expression).leftOperand) && rightOperand.equals(((Operation) expression).rightOperand);
    }

}
