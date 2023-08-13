"use strict";


// Only 3?
let operations = f => (...args) => (x, y, z) => {
    return f(...args.map(a => a(x, y, z)));
}

let variable = name => (x, y, z)  => {
    return name === "x" ? x : name === "y" ? y : name === "z" ? z : NaN;
}

// Ignore
let cnst = num => _ => num;
let add = operations((l, r) => l + r);
let subtract = operations((l, r) => l - r);
let divide = operations((l, r) => l / r);
let multiply = operations((l, r) => l * r);
let negate = operations(v => -v);

let sinh = operations(Math.sinh);
let cosh = operations(v => Math.cosh(v));

let one = cnst(1);
let two = cnst(2);
