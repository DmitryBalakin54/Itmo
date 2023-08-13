package expression.generic;

import expression.exceptions.CalculationException;
import expression.exceptions.ParseException;
import expression.generic.expressionoperations.TripleExpression;
import expression.generic.operations.*;
import expression.generic.parser.GenericExpressionParser;


import java.util.Map;

public class GenericTabulator implements Tabulator {

    private final Map<String, GenericOperations<?>> mods = Map.of(
            "i", new CheckedIntegerOperations(),
            "d", new DoubleOperations(),
            "bi", new BigIntegerOperations(),
            "f", new FloatOperations(),
            "s", new ShortOperations(),
            "u", new IntegerOperations()
    );

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        if (x1 > x2 || y1 > y2 || z1 > z2) {
            throw new IllegalArgumentException("Illegal arguments");
        }
        return tabulate(mods.get(mode), expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] tabulate(GenericOperations<T> operations, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        Object[][][] result = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        GenericExpressionParser<T> parser = new GenericExpressionParser<>(operations);
        TripleExpression<T> expr;
        try {
            expr = parser.parse(expression);
        } catch (ParseException e) {
            return result;
        }


        for (int x = 0; x <= x2 - x1; x++) {
            for (int y = 0; y <= y2 - y1; y++) {
                for (int z = 0; z <= z2 - z1; z++) {
                    try {
                        result[x][y][z] = expr.evaluate(
                                operations.parseNum(String.valueOf(x + x1)),
                                operations.parseNum(String.valueOf(y + y1)),
                                operations.parseNum(String.valueOf(z + z1))
                        );
                    } catch (CalculationException e) {
                        result[x][y][z] = null;
                    }
                }
            }
        }
        return result;
    }
}
