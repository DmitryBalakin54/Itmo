package expression;

import java.util.List;

public class Variable extends ExpressionObject implements MultiExpression {

    private final List<String> SYMBOLS = List.of("x", "y", "z");

    private final String value;
    public Variable(String value) {
        this.value = value;
        for (String str : SYMBOLS) {
            if (value.equals(str)) {
                return;
            }
        }
        try {
            throw new Exception("Illegal value : " + value);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public int evaluate(int x) {
            return x;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        if (value.equals("x")) {
            return x;
        } else if (value.equals("y")) {
            return y;
        } else {
            return z;
        }
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object expression) {
        if (expression == null || this.getClass() != expression.getClass()) {
            return false;
        }

        return value.equals(((Variable) expression).value);
    }
}
