package expression.parser;

import expression.*;

import java.util.Map;

public class ExpressionParser implements TripleParser {

    private final Map<ExpressionToken, Integer> sighPriority = Map.of(  // here
            ExpressionToken.NONE, -1,
            ExpressionToken.SET, 0,
            ExpressionToken.CLEAR, 0,
            ExpressionToken.SUB, 1,
            ExpressionToken.ADD, 1,
            ExpressionToken.MULTIPLY, 2,
            ExpressionToken.DiVIDE, 2,
            ExpressionToken.NEGATE, 3
    );
    private  String expression;
    private int pos;
    private char ch;


    public ExpressionParser() {
    }

    @Override
    public TripleExpression parse(String expression) {
        if (expression.length() == 0) {
            return null;
        }
        this.expression = expression;
        pos = 0;
//        System.err.println("----------------------");
//        System.err.println(expression);
//        System.err.println("----------------------");
        return parsePart(ch == '(', ExpressionToken.NONE ,ExpressionToken.NONE);
    }

    private MultiExpression parsePart(boolean beginIsBracket, ExpressionToken lastSign, ExpressionToken lastToken) {
        MultiExpression result = null;
        ExpressionToken currentSign = ExpressionToken.NONE;
        while (hasNext()) {
            ch = take(pos++);
            if ((ch == '-' && Character.isDigit(take(pos)) && (isSign(lastToken) || lastToken == ExpressionToken.NONE) || Character.isDigit(ch))) {
                if (result == null) {
                    StringBuilder num = new StringBuilder();
                    num.append(ch);
                    while (hasNext()){
                        ch = take(pos++);
                        if (Character.isDigit(ch)) {
                            num.append(ch);
                        } else {
                            pos--;
                            break;
                        }
                    }
                    result = new Const(Integer.parseInt(num.toString()));
                    lastToken = ExpressionToken.CONST;
                } else {
                    pos--;
                    result = makeExpression(result, parsePart(false, currentSign, ExpressionToken.CONST), currentSign);
                    lastToken = ExpressionToken.CONST;
                }
            } else if (ch =='x' || ch == 'y' || ch =='z') {
                result = new Variable(Character.toString(ch));
                lastToken = ExpressionToken.VARIABLE;
            } else if (ch == 's' || ch == 'c' || ch == '+' || ch == '*' || ch == '/' ||
                    (ch == '-' && (lastToken == ExpressionToken.CONST || lastToken == ExpressionToken.VARIABLE) )) { // here
                StringBuilder sign = new StringBuilder(Character.toString(ch));
                if (ch == 's' || ch == 'c') {
                    while (hasNext()) {
                        ch = take(pos++);
                        sign.append(ch);
                        if (ch == 't' || ch == 'r') {
                            break;
                        }
                    }
                }
                ExpressionToken signToken = setExpressionToken(sign.toString());
                if (leftOperationLessRight(lastSign, signToken) || beginIsBracket) {
                    result = makeExpression(result, parsePart(false, signToken, signToken), signToken);
                    lastToken = ExpressionToken.VARIABLE;
                } else {
                    pos -= sign.length();
                    return result;
                }
            } else if (ch == '-') {
                lastToken = ExpressionToken.NEGATE;
                currentSign = ExpressionToken.NEGATE;
                result = new Negate(parsePart(false, currentSign, lastToken));
                lastToken = ExpressionToken.VARIABLE;
            } else if (ch == '(') {
                System.err.println(lastToken);
                if (result != null) {
                    return parsePart(true, lastSign, lastToken);
                } else {
                    result = parsePart(true, lastSign, lastToken);
                    lastToken = ExpressionToken.VARIABLE;
                }
            } else if (ch == ')') {
                if (!beginIsBracket) {
                    pos--;
                }
                return result;
            }
        }
        return result;
    }

    private MultiExpression makeExpression( MultiExpression left, MultiExpression right, ExpressionToken operation) { // here
        if (operation == ExpressionToken.ADD) {
            return new Add(left, right);
        } else if (operation == ExpressionToken.SUB) {
            return new Subtract(left, right);
        } else if (operation == ExpressionToken.MULTIPLY) {
            return new Multiply(left, right);
        } else if (operation == ExpressionToken.DiVIDE) {
            return new Divide(left, right);
        } else if (operation == ExpressionToken.SET) {
            return new Set(left, right);
        } else if (operation == ExpressionToken.CLEAR) {
            return new Clear(left, right);
        } else {
            return null;
        }
    }

    private char take(int position) {
        return expression.charAt(position);
    }

    private boolean hasNext() {
        return pos < expression.length();
    }

    private boolean leftOperationLessRight(ExpressionToken left, ExpressionToken right) {
        return sighPriority.get(left) < sighPriority.get(right);
    }

    private boolean isSign(ExpressionToken token) { // here
        return token == ExpressionToken.DiVIDE || token == ExpressionToken.ADD ||
                token == ExpressionToken.MULTIPLY || token == ExpressionToken.SUB ||
                token == ExpressionToken.NEGATE || token == ExpressionToken.SET ||
                token == ExpressionToken.CLEAR;
    }
    private ExpressionToken setExpressionToken(String token) { // here
        if (token.equals("+")) {
            return ExpressionToken.ADD;
        } else if (token.equals("*")) {
            return ExpressionToken.MULTIPLY;
        } else if (token.equals("/")) {
            return ExpressionToken.DiVIDE;
        } else if (token.equals("-")) {
            return ExpressionToken.SUB;
        } else if (token.equals("x") || token.equals("y") || token.equals("z")) {
            return ExpressionToken.VARIABLE;
        } else if (token.equals("set")) {
            return ExpressionToken.SET;
        } else if (token.equals("clear")) {
            return ExpressionToken.CLEAR;
        }  else {
            return ExpressionToken.CONST;
        }
    }
}
