package expression.generic.operations;

import expression.exceptions.DivisionByZeroException;
import expression.exceptions.OverflowException;

public class CheckedIntegerOperations implements GenericOperations<Integer> {
    @Override
    public Integer add(Integer left, Integer right) {
        if (left > 0 && Integer.MAX_VALUE - left < right || right < 0 && Integer.MIN_VALUE - right > left ) {
            throw new OverflowException(toString());
        }
        return left + right;
    }

    @Override
    public Integer subtract(Integer left, Integer right) {
        if (right < 0 &&  left > Integer.MAX_VALUE + right || right > 0 && left  < Integer.MIN_VALUE + right) {
            throw new OverflowException(toString());
        }
        return left - right;
    }

    @Override
    public Integer divide(Integer left, Integer right) {
        if (right == 0) {
            throw new DivisionByZeroException(toString());
        }
        if (left == Integer.MIN_VALUE && right == -1) {
            throw new OverflowException(toString());
        }
        return left / right;
    }

    @Override
    public Integer multiply(Integer left, Integer right) {
        if (left == Integer.MIN_VALUE && right == -1 ||
                right == Integer.MIN_VALUE && left == -1 ||
                left > 0 && right > 0 && left > Integer.MAX_VALUE / right ||
                left < 0 && right < 0 && left < Integer.MAX_VALUE / right ||
                left > 0 && right < 0 && right < Integer.MIN_VALUE / left ||
                left < 0 && right > 0 && left < Integer.MIN_VALUE / right) {
            throw new OverflowException(toString());
        }
        return left * right;
    }

    @Override
    public Integer negate(Integer operand) {
        if (operand == Integer.MIN_VALUE) {
            throw new OverflowException(toString());
        }
        return - operand;
    }

    @Override
    public Integer parseNum(String num) {
        return Integer.parseInt(num);
    }
}
