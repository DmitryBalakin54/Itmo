package expression.generic.operations;

import expression.exceptions.DivisionByZeroException;
import expression.exceptions.OverflowException;

public class FloatOperations implements GenericOperations<Float>{
    @Override
    public Float add(Float left, Float right) {
        return left + right;
    }

    @Override
    public Float subtract(Float left, Float right) {
        return left - right;
    }

    @Override
    public Float divide(Float left, Float right) {
        return left / right;
    }

    @Override
    public Float multiply(Float left, Float right) {
        return left * right;
    }

    @Override
    public Float negate(Float operand) {
        return -operand;
    }

    @Override
    public Float parseNum(String num) {
        return Float.parseFloat(num);
    }
}
