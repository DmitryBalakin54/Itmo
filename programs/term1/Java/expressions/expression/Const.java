package expression;

public class Const extends ExpressionObject implements MultiExpression {

    private final int value;
    public Const(int value) {
        this.value = value;
    }

    @Override
    public int evaluate(int x) {
        return value;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return evaluate(x);
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @Override
    public boolean equals(Object expression) {
        if (expression == null || this.getClass() != expression.getClass()) {
            return false;
        }

        return value == ((Const) expression).value;
    }
}
