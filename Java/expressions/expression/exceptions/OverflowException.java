package expression.exceptions;

public class OverflowException extends CalculationException {
    public OverflowException(String message) {
        super("Overflow in " + message);
    }
}
