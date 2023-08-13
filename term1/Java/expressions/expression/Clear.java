package expression;

public class Clear extends Operation implements MultiExpression {

    public Clear(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    public int makeOperation(int left, int right) {
        return left & ~(1 << right);
    }

    @Override
    protected String setSign() {
        return "clear";
    }
}
