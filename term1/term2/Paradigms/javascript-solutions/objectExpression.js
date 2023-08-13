"use strict"

let operation = {
    values: null,
    evaluate: function (...args) {
        return this.makeOperation(...this.values.map(a => a.evaluate(...args)))
    },
    makeOperation: () => null,
    setSign: function () {
        return ""
    },
    toString: function () {
        return this.values.map(expr => expr.toString()).join(" ") + " " + this.setSign()
    },
    prefix: function () {
        return "(" + this.setSign() + " " + this.values.map(expr => expr.prefix()).join(" ") + ")"
    }
}

let object = {
    value: null,
    evaluate: function () {
        return this.value
    },
    toString: function () {
        return this.value + ""
    },
    prefix: function () {
        return this.toString()
    }
}

let opConstructor = f => sign => function (...args) {
    let res = Object.create(operation)
    res.values = args
    res.makeOperation = f
    res.setSign = () => sign
    return res
}

let objectConstr = function (...value) {
    let res = Object.create(object)
    res.value = value[0]
    return res
}

const variables = {"x": 0, "y": 1, "z": 2}

let variableConstructor = function (...value) {
    let res = objectConstr(...value);
    res.evaluate = function (...values) {
        return values[variables[res.value]]
    }
    return res
}

let reduce = function (...array) {return array.reduce((left, right) => left + right, 0)}

let Const = objectConstr
let Variable = variableConstructor
let Add = opConstructor((left, right) => left + right)("+")
let Subtract = opConstructor((left, right) => left - right)("-")
let Multiply = opConstructor((left, right) => left * right)("*")
let Divide = opConstructor((left, right) => left / right)("/")
let Negate = opConstructor(value => -value)("negate")
let ArcTan = opConstructor(value => Math.atan(value))("atan")
let ArcTan2 = opConstructor((left, right) => Math.atan2(left, right))("atan2")
let Sum = opConstructor((...args) => reduce(...args))("sum")
let Avg = opConstructor((...args) => reduce(...args) / args.length)("avg")

let shift = (count, array) => {
    let res = []
    for (let i = 0; i < count; i++) {
        res.unshift(array.pop())
    }
    return res
}

const VARIABLES = ["x", "y", "z"]
const OPERATIONS = ["+", "-", "*", "/", "negate", "atan", "atan2", "sum", "avg"]
const OPERATIONS_MAP = {
    "+": [Add, 2], "-": [Subtract, 2], "*": [Multiply, 2], "/": [Divide, 2], "negate": [Negate, 1],
    "atan": [ArcTan, 1], "atan2": [ArcTan2, 2], "sum": [Sum, -1], "avg": [Avg, -1]
}

let parse = function (expression) {
    expression = expression.split(" ").filter(ch => ch.length > 0)
    let stack = []
    for (let ch of expression) {
        if (VARIABLES.includes(ch)) {
            stack.push(new Variable(ch))
        } else if (OPERATIONS.includes(ch)) {
            stack.push(new OPERATIONS_MAP[ch][0](...shift(OPERATIONS_MAP[ch][1], stack)))
        } else {
            stack.push(new Const(Number.parseInt(ch)))
        }
    }
    return stack.shift()
}


function ParseError(excepted, actual) {
    this.message = "Excepted: " + excepted + ", Actual: " + actual
}
ParseError.prototype = Object.create(Error.prototype)
ParseError.prototype.name = "ParserError"
ParseError.prototype.constructor = ParseError;


let beginBracket = "("
let endBracket = ")"
let isNum = string => !!Number(string) || string === "0";
let rebuildString = string => {
    let res = ""
    for (let ch of string) {
        if (ch === beginBracket || ch === endBracket) {
            res += (" " + ch + " ")
        } else {
            res += ch
        }
    }
    return res;
}

let parseVariableOrConst = token => {
    if (VARIABLES.includes(token)) {
        return new Variable(token)
    } else if (isNum(token)) {
        return new Const(Number.parseInt(token))
    } else {
        throw new ParseError("( or variable or number", token)
    }
}

let parsePrefixPart = function (array, index) {
    let token = array[index++]
    const stack = []
    while (index < array.length - 1) {
        if (array[index] === endBracket) {
            break;
        }
        if (array[index] === beginBracket) {
            const res = parsePrefixPart(array, ++index)
            index = res[1]
            stack.push(res[0])
            continue
        }
        stack.push(parseVariableOrConst(array[index]))
        index++
    }
    if (array[index] !== endBracket) {
        throw new ParseError(endBracket, array[index])
    }

    if (!OPERATIONS.includes(token)) {
        throw new ParseError("operation", token)
    }
    const argsAmount = OPERATIONS_MAP[token][1]
    const operation = OPERATIONS_MAP[token][0]

    if (argsAmount === -1 || stack.length === argsAmount) {
        return [new operation(...stack), ++index]
    } else {
        throw new ParseError(argsAmount + " arguments for operation " + token, stack.length)
    }
}


let parsePrefix = function (string) {
    string = rebuildString(string)
    string = string.split(" ").filter(ch => ch.length > 0)
    if (string.length === 0) {
        throw new ParseError("expression", "empty string")
    }

    const token = string[0]
    if (string.length === 1) {
        return parseVariableOrConst(token)
    } else if (string[0] !== beginBracket) {
        throw new ParseError(beginBracket, token)
    } else {
        const res = parsePrefixPart(string, 1)
        if (res[1] !== string.length) {
            throw new ParseError("end of expression", string[res[1]])
        }
        return res[0];
    }
}