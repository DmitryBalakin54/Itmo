package expression.generic.operations;

import expression.exceptions.DivisionByZeroException;

public class IntegerOperations implements GenericOperations<Integer> {
    @Override
    public Integer add(Integer left, Integer right) {
        return left + right;
    }

    @Override
    public Integer subtract(Integer left, Integer right) {
        return left - right;
    }

    @Override
    public Integer divide(Integer left, Integer right) {
        if (right == 0) {
            throw new DivisionByZeroException(toString());
        }

        return left / right;
    }

    @Override
    public Integer multiply(Integer left, Integer right) {
        return left * right;
    }

    @Override
    public Integer negate(Integer operand) {
        return -operand;
    }

    @Override
    public Integer parseNum(String num) {
        return Integer.parseInt(num);
    }
}
