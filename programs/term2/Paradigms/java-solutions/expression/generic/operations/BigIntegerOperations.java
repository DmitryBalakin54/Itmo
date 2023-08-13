package expression.generic.operations;

import expression.exceptions.DivisionByZeroException;
import expression.exceptions.OverflowException;

import java.math.BigInteger;
import java.util.Objects;

public class BigIntegerOperations implements GenericOperations<BigInteger> {
    @Override
    public BigInteger add(BigInteger left, BigInteger right) {
        return left.add(right);
    }

    @Override
    public BigInteger subtract(BigInteger left, BigInteger right) {
        return left.subtract(right);
    }

    @Override
    public BigInteger divide(BigInteger left, BigInteger right) {
        if (Objects.equals(right, BigInteger.ZERO)) {
            throw new DivisionByZeroException(toString());
        }
        return left.divide(right);
    }

    @Override
    public BigInteger multiply(BigInteger left, BigInteger right) {
        return left.multiply(right);
    }

    @Override
    public BigInteger negate(BigInteger operand) {
        return operand.negate();
    }

    @Override
    public BigInteger parseNum(String num) {
        return new BigInteger(num);
    }
}
