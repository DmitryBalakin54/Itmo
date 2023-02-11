package expression.exceptions;

public class IntegerOverflowException extends ParseException{
    public IntegerOverflowException(String message) {
        super("String " + message + " is not an integer");
    }
}
