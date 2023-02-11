package expression.exceptions;

import expression.TripleExpression;

public class Main {
    public static void main(String[] args) throws Exception {
        ExpressionParser parser = new ExpressionParser();
        String s = "count 3";
        TripleExpression expr = parser.parse(s);
        System.out.println(expr);
        //System.out.println(expr.evaluate(0, -2147483648, 0));
    }
}
