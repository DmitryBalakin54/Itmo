package expression;

public class Divide extends Operation implements MultiExpression {

    public Divide(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    public int makeOperation(int left, int right) {
        return left / right;
    }

    @Override
    protected String setSign() {
        return "/";
    }
}
