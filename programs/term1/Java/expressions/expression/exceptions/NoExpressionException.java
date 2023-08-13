package expression.exceptions;

public class NoExpressionException extends ParseException{
    public NoExpressionException() {
        super("It's not an expression");
    }
}
