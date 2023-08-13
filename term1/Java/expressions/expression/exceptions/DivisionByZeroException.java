package expression.exceptions;

public class DivisionByZeroException extends CalculationException {
    public DivisionByZeroException(String message) {
        super("Division by zero in " + message);
    }
}
