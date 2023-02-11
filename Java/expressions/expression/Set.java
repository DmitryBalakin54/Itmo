package expression;

public class Set extends Operation implements MultiExpression{

    public Set(MultiExpression leftOperand, MultiExpression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    public int makeOperation(int left, int right) {
        return left | (1 << right);
    }

    @Override
    protected String setSign() {
        return "set";
    }
}
