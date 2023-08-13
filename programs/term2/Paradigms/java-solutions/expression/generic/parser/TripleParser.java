package expression.generic.parser;

import expression.generic.expressionoperations.TripleExpression;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FunctionalInterface
public interface TripleParser<T> {
    TripleExpression<T> parse(String expression) throws Exception;
}
