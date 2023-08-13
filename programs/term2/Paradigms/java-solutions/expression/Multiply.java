package expression;

public class Multiply extends Operation implements MultiExpression {

    public Multiply(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    public int makeOperation(int left, int right) {
        return left * right;
    }

    @Override
    protected String setSign() {
        return "*";
    }
}
