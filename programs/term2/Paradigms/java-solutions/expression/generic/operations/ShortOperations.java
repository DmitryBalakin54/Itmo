package expression.generic.operations;

import expression.exceptions.DivisionByZeroException;

public class ShortOperations implements GenericOperations<Short> {
    @Override
    public Short add(Short left, Short right) {
        return (short) (left + right);
    }

    @Override
    public Short subtract(Short left, Short right) {
        return (short) (left - right);
    }

    @Override
    public Short divide(Short left, Short right) {
        if (right == 0) {
            throw new DivisionByZeroException(toString());
        }

        return (short) (left / right);
    }

    @Override
    public Short multiply(Short left, Short right) {
        return (short) (left * right);
    }

    @Override
    public Short negate(Short operand) {
        return (short) -operand;
    }

    @Override
    public Short parseNum(String num) {
        return (short) Integer.parseInt(num);
    }
}
