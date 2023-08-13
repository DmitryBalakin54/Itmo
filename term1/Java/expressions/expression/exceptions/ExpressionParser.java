package expression.exceptions;

import expression.*;
import expression.parser.ExpressionToken;
import expression.parser.TripleParser;

import java.util.Map;
import java.util.Stack;

public class ExpressionParser implements TripleParser {

    private final Map<ExpressionToken, Integer> sighPriority = Map.of(  // here
            ExpressionToken.NONE, 0,
            ExpressionToken.SET, 1,
            ExpressionToken.CLEAR, 1,
            ExpressionToken.SUB, 2,
            ExpressionToken.ADD, 2,
            ExpressionToken.MULTIPLY, 3,
            ExpressionToken.DiVIDE, 3,
            ExpressionToken.NEGATE, 4,
            ExpressionToken.COUNT, 4

    );
    private  String expression;
    private int pos;
    private char ch;

    private final Stack<Integer> bracket = new Stack<>();


    public ExpressionParser() {
    }

    @Override
    public TripleExpression parse(String expression) {
        if (expression.length() == 0) {
            return null;
        }
        this.expression = expression;
        pos = 0;
        bracket.clear();
//        System.err.println("----------------------");
//        System.err.println(expression);
//        System.err.println("----------------------");
        TripleExpression result =  parsePart(ch == '(', ExpressionToken.NONE ,ExpressionToken.NONE);

        if (hasNext()) {
            throw new InvalidCharacterException(ch, pos - 1);
        } else if (!bracket.isEmpty()) {
            throw new InvalidCharacterException('(', bracket.pop());
        } else if (result == null) {
            throw new NoExpressionException();
        } else {
            return result;
        }
    }

    private MultiExpression parsePart(boolean beginIsBracket, ExpressionToken lastSign, ExpressionToken lastToken) {
        MultiExpression result = null;
        ExpressionToken currentSign = ExpressionToken.NONE;
        while (hasNext()) {
            ch = take(pos++);
            if (isInvalidChar(ch)) {
                if (ch == 't' || ch == 'r') {
                    if (pos > 1) {
                        if (ch == 't' && take(pos - 2) != 'e' || ch == 'r' && take(pos - 2) != 'a') {
                            throw new InvalidCharacterException(ch, pos - 1);
                        }
                    } else {
                        throw new InvalidCharacterException(ch, pos - 1);
                    }
                } else {
                    throw new InvalidCharacterException(ch, pos - 1);
                }
            }
            if (ch == '-' && isNegateNum(lastToken)|| Character.isDigit(ch)) {
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
                    try {
                        result = new Const(Integer.parseInt(num.toString()));
                    } catch (NumberFormatException e) {
                        throw new IntegerOverflowException(num.toString());
                    }
                    lastToken = ExpressionToken.CONST;
                } else {
                    pos--;
                    result = makeExpression(result, parsePart(false, currentSign, ExpressionToken.CONST), currentSign);
                    lastToken = ExpressionToken.CONST;
                }
            } else if (ch =='x' || ch == 'y' || ch =='z') {
                result = new Variable(Character.toString(ch));
                lastToken = ExpressionToken.VARIABLE;
            } else if ( ch == 's' || ch == 'c' && nextIs('l')|| ch == '+' || ch == '*' || ch == '/' || (ch == '-' && (lastToken == ExpressionToken.CONST || lastToken == ExpressionToken.VARIABLE) )) {
                if (lastToken != ExpressionToken.VARIABLE && lastToken != ExpressionToken.CONST) {
                    throw new InvalidCharacterException(ch, pos - 1);
                }
                String sign = parseOperation();
                ExpressionToken signToken = setExpressionToken(sign);
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
                result = new CheckedNegate(parsePart(false, currentSign, lastToken));
                lastToken = ExpressionToken.VARIABLE;
            } else if (ch == '(') {
                bracket.push(pos - 1);
                if (!isSign(lastToken) && lastToken != ExpressionToken.NONE) {
                    throw new InvalidCharacterException(ch, pos - 1);
                }
                if (result != null) {
                    return parsePart(true, lastSign, lastToken);
                } else {
                    result = parsePart(true, lastSign, lastToken);
                    lastToken = ExpressionToken.VARIABLE;
                }
            } else if (ch == ')') {
                if (result == null) {
                    throw new InvalidCharacterException(ch, pos - 1);
                }
                if (!beginIsBracket) {
                    pos--;
                } else {
                    if (bracket.isEmpty()) {
                        throw new InvalidCharacterException(ch, pos - 1);
                    }else {
                        bracket.pop();
                    }
                }
                return result;
            } else if (ch == 'c') {
                String sign = patternPars("count");
                lastToken = ExpressionToken.COUNT;
                currentSign = ExpressionToken.COUNT;
                result = new CheckedCount(parsePart(false, currentSign, lastToken));
                lastToken = ExpressionToken.VARIABLE;
            }
        }
        if (!hasNext() && lastToken != ExpressionToken.VARIABLE && lastToken != ExpressionToken.CONST && lastToken != ExpressionToken.COUNT) {
            throw new InvalidCharacterException(ch, pos - 1);
        }

        return result;
    }

    private String parseOperation() {
        if (ch == 's') {
            return patternPars("set");
        } else if (ch == 'c') {
            return patternPars("clear");
        }
        return Character.toString(ch);
    }

    private String patternPars(final String pattern) {
        StringBuilder res = new StringBuilder();
        int index = 0;
        if (pos > 1) {
            char lastCh = take(pos - 2);
            if (Character.isDigit(lastCh) || lastCh == 'x' || lastCh == 'y' || lastCh == 'z') {
                throw new InvalidCharacterException(take(pos - 2), pos - 2);
            }
        }
        while (hasNext() && index < pattern.length()) {
            if (ch != pattern.charAt(index)) {
                throw new InvalidCharacterException(ch, pos - 1);
            }
            res.append(ch);
            ch = take(pos++);
            index++;
        }
        if (index != pattern.length() || (Character.isDigit(ch) || ch == 'x' || ch == 'y' || ch == 'z')) {
            throw new InvalidCharacterException(ch, pos - 1);
        }
        pos--;
        return res.toString();
    }
    private boolean isNegateNum(ExpressionToken lastToken) {
        if (hasNext() && ch == '-') {
            if (!(Character.isDigit(take(pos)) && (isSign(lastToken) || lastToken == ExpressionToken.NONE))) {
                return false;
            }
        } else if (!hasNext() && ch == '-') {
            return false;
        }
        return true;
    }
    private boolean isInvalidChar(char ch) {
        return !(ch =='s' || ch == 'c' || ch == 'x' || ch == 'y' || ch == 'z' || Character.isDigit(ch) || ch == '+' || ch == '-' || ch == '*' || ch == '/' || ch == '(' || ch == ')' || Character.isWhitespace(ch));
    }
    private MultiExpression makeExpression( MultiExpression left, MultiExpression right, ExpressionToken operation) {
        if (operation == ExpressionToken.ADD) {
            return new CheckedAdd(left, right);
        } else if (operation == ExpressionToken.SUB) {
            return new CheckedSubtract(left, right);
        } else if (operation == ExpressionToken.MULTIPLY) {
            return new CheckedMultiply(left, right);
        } else if (operation == ExpressionToken.DiVIDE) {
            return new CheckedDivide(left, right);
        } else if (operation == ExpressionToken.SET) {
            return new CheckedSet(left, right);
        } else if (operation == ExpressionToken.CLEAR) {
            return new CheckedClear(left, right);
        } else {
            return null;
        }
    }


    private char take(int position) {
        return expression.charAt(position);
    }

    private boolean nextIs(char character) {
        if (hasNext()) {
            return take(pos) == character;
        } else {
            return false;
        }
    }
    private boolean hasNext() {
        return pos < expression.length();
    }

    private boolean leftOperationLessRight(ExpressionToken left, ExpressionToken right) {
        return sighPriority.get(left) < sighPriority.get(right);
    }

    private boolean isSign(ExpressionToken token) {
        return token == ExpressionToken.DiVIDE || token == ExpressionToken.ADD ||
                token == ExpressionToken.MULTIPLY || token == ExpressionToken.SUB ||
                token == ExpressionToken.NEGATE || token == ExpressionToken.SET ||
                token == ExpressionToken.CLEAR || token == ExpressionToken.COUNT;
    }
    private ExpressionToken setExpressionToken(String token) {
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
