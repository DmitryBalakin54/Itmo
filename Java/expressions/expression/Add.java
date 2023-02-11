package expression;

public class Add extends Operation implements MultiExpression {

    public Add(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    public int makeOperation(int left, int right) {
        return left + right;
    }

    @Override
    protected String setSign() {
        return "+";
    }
}
