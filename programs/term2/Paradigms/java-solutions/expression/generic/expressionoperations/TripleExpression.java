package expression.generic.expressionoperations;

import base.Pair;
import base.TestCounter;
import expression.common.ExpressionKind;

import java.util.List;

/**
 * Three-argument arithmetic expression over integers.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */

public interface TripleExpression<T> extends ToMiniString {
    T evaluate(T x, T y, T z);
}
