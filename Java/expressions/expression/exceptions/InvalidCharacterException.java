package expression.exceptions;

public class InvalidCharacterException extends ParseException {
    public InvalidCharacterException(String message) {
        super(message);
    }
    public InvalidCharacterException(char ch, int pos) {
        super("Invalid character " + ch + " at position " + pos);
    }
}
