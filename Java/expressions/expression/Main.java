package expression;


import expression.parser.ExpressionParser;
import expression.parser.TripleParser;

public class Main {
    public static void main(String[] args) {
       ExpressionParser parser = new ExpressionParser();
       String s = "z set y set (y set x)";
       TripleExpression expr = parser.parse(s);
        System.out.println(expr);
    }
}
