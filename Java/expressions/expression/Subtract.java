package expression;

public class Subtract extends Operation implements MultiExpression {

    public Subtract(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    public int makeOperation(int left, int right) {
        return left - right;
    }

    @Override
    protected String setSign() {
        return "-";
    }
}
