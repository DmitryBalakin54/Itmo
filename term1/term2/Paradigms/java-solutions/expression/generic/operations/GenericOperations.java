package expression.generic.operations;

public interface GenericOperations<T> {

    T add(T left, T right);

    T subtract(T left, T right);

    T divide(T left, T right);

    T multiply(T left, T right);

    T negate(T operand);

    T parseNum(String num);

}
