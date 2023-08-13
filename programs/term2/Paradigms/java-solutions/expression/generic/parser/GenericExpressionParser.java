package expression.generic.parser;

import expression.generic.expressionoperations.*;
import expression.generic.expressionoperations.standartoperations.*;
import expression.exceptions.IntegerOverflowException;
import expression.exceptions.InvalidCharacterException;
import expression.exceptions.NoExpressionException;
import expression.generic.operations.GenericOperations;
import expression.parser.ExpressionToken;

import java.util.Map;
import java.util.Stack;

public class GenericExpressionParser<T> implements TripleParser<T> {

    private final Map<ExpressionToken, Integer> sighPriority = Map.of(  // here
            ExpressionToken.NONE, -1,
            ExpressionToken.SUB, 2,
            ExpressionToken.ADD, 2,
            ExpressionToken.MULTIPLY, 3,
            ExpressionToken.DiVIDE, 3,
            ExpressionToken.NEGATE, 4

    );
    private  String expression;
    private int pos;
    private char ch;

    private final GenericOperations<T> operation;

    private final Stack<Integer> bracket = new Stack<>();


    public GenericExpressionParser(GenericOperations<T> operation) {
        this.operation = operation;
    }

    @Override
    public TripleExpression<T> parse(String expression) {
        if (expression.length() == 0) {
            return null;
        }
        this.expression = expression;
        pos = 0;
        bracket.clear();
//        System.err.println("----------------------");
//        System.err.println(expression);
//        System.err.println("----------------------");
        TripleExpression<T> result =  parsePart(ch == '(', ExpressionToken.NONE ,ExpressionToken.NONE);

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

    private MultiExpression<T> parsePart(boolean beginIsBracket, ExpressionToken lastSign, ExpressionToken lastToken) {
        MultiExpression<T> result = null;
        ExpressionToken currentSign = ExpressionToken.NONE;
        while (hasNext()) {
            ch = take(pos++);
//            if (isInvalidChar(ch)) {
//                if (ch == 't' || ch == 'r') {
//                    if (pos > 1) {
//                        if (ch == 't' && take(pos - 2) != 'e' || ch == 'r' && take(pos - 2) != 'a') {
//                            throw new InvalidCharacterException(ch, pos - 1);
//                        }
//                    } else {
//                        throw new InvalidCharacterException(ch, pos - 1);
//                    }
//                } else {
//                    throw new InvalidCharacterException(ch, pos - 1);
//                }
//            }
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
                        result = new Const<T>(operation.parseNum(num.toString()));
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
                result = new Variable<T>(Character.toString(ch));
                lastToken = ExpressionToken.VARIABLE;
            }  else if (ch == '(') {
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
            } else  if (!Character.isWhitespace(ch)){
//                if (lastToken != ExpressionToken.VARIABLE && lastToken != ExpressionToken.CONST) {
//                    throw new InvalidCharacterException(ch, pos - 1);
//                }
                String sign = parseOperation();
                ExpressionToken signToken = setExpressionToken(sign);
                if (!isUnary(sign, lastToken) && (lastToken == ExpressionToken.CONST || lastToken == ExpressionToken.VARIABLE)) {
                    if (leftOperationLessRight(lastSign, signToken) || beginIsBracket) {
                        result = makeExpression(result, parsePart(false, signToken, signToken), signToken);
                        lastToken = ExpressionToken.VARIABLE;
                    } else {
                        pos -= sign.length();
                        return result;
                    }
                } else if (isUnary(sign, lastToken)) {
                    lastToken = setUnaryExpressionToken(sign);
                    currentSign = setUnaryExpressionToken(sign);
                    result = makeUnaryExpression(parsePart(false, currentSign, lastToken), currentSign);
                    lastToken = ExpressionToken.VARIABLE;
                } else {
                    throw new InvalidCharacterException(ch, pos - 1);
                }
            }
        }
        if (!hasNext() && lastToken != ExpressionToken.VARIABLE && lastToken != ExpressionToken.CONST && lastToken != ExpressionToken.COUNT) {
            throw new InvalidCharacterException(ch, pos - 1);
        }

        return result;
    }

    private boolean isUnary(String sign, ExpressionToken lastToken) {
        return sign.equals("-") && lastToken != ExpressionToken.VARIABLE && lastToken != ExpressionToken.CONST || sign.equals("count");
    }
    private String parseOperation() {
        if (ch == 's') {
            if (patternPars("set")) {
                return "set";
            }
        } else if (ch == 'c') {
            if ( patternPars("clear")) {
                return "clear";
            } else if (patternPars("count")){
                return "count";
            }
        }
        return Character.toString(ch);
    }

    private boolean patternPars(final String pattern) {
        StringBuilder res = new StringBuilder();
        int index = 0;
        int subPos = pos;
        char subCh = ch;
        if (pos > 1) {
            char lastCh = take(pos - 2);
            if (Character.isDigit(lastCh) || lastCh == 'x' || lastCh == 'y' || lastCh == 'z') {
                throw new InvalidCharacterException(take(pos - 2), pos - 2);
            }
        }
        while (hasNext() && index < pattern.length()) {
            if (ch != pattern.charAt(index)) {
                pos = subPos;
                ch = subCh;
                return false;
            }
            res.append(ch);
            ch = take(pos++);
            index++;
        }
        if (index != pattern.length() || (Character.isDigit(ch) || ch == 'x' || ch == 'y' || ch == 'z')) {
            pos = subPos;
            ch = subCh;
            return false;
        }
        pos--;
        return true;
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
    private MultiExpression<T> makeExpression( MultiExpression<T> left, MultiExpression<T> right, ExpressionToken operation) {
        if (operation == ExpressionToken.ADD) {
            return new Add<T>(left, right, this.operation);
        } else if (operation == ExpressionToken.SUB) {
            return new Subtract<T>(left, right, this.operation);
        } else if (operation == ExpressionToken.MULTIPLY) {
            return new Multiply<T>(left, right, this.operation);
        } else if (operation == ExpressionToken.DiVIDE) {
            return new Divide<T>(left, right, this.operation);
        } else {
            return null;
        }
    }

    private MultiExpression<T> makeUnaryExpression( MultiExpression<T> expr, ExpressionToken operation) {
        if (operation == ExpressionToken.NEGATE) {
            return new Negate<T>(expr, this.operation);
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
                token == ExpressionToken.NEGATE;
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
        } else {
            throw new InvalidCharacterException(ch, pos - 1);
        }
    }

    private ExpressionToken setUnaryExpressionToken(String token) {
        if (token.equals("-")) {
            return ExpressionToken.NEGATE;
        } else {
            return null;
        }
    }
}
