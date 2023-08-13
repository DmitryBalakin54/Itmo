package expression.generic.operations;

public class DoubleOperations implements GenericOperations<Double> {
    @Override
    public Double add(Double left, Double right) {
        return left + right;
    }

    @Override
    public Double subtract(Double left, Double right) {
        return left - right;
    }

    @Override
    public Double divide(Double left, Double right) {
        return left / right;
    }

    @Override
    public Double multiply(Double left, Double right) {
        return left * right;
    }

    @Override
    public Double negate(Double operand) {
        return - operand;
    }

    @Override
    public Double parseNum(String num) {
        return Double.parseDouble(num);
    }
}
