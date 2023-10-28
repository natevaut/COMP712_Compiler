#lang eopl
(require "./Interpreter.rkt")

(display (run #<<ENDOFTEXT

// COMP712 -- Programming Languages
// Some tests for the interpreter of the AJS language
// Note: students may want to provide additional tests
// 

//=========================
console.log("Simple arithmetic tests");
//=========================

console.log(345);

console.log(123 + 45);

console.log(1000 - 482);

console.log(5 * 35);

console.log(10 / 4);

console.log(9.5 + 22);

console.log((3 * 5) + (10 -4));

console.log(1 - 5 / 2 * 4 + 3);

console.log(3 * 2 * (5 + 1 - 2));


//============================
console.log("Test constant declarations");
//============================

const size = 5;
console.log(size);
console.log(5 * size);

const pi = 3.14159;
const radius = 10;
console.log(pi * radius * radius);

const circumference = 2 * pi * radius;
console.log(circumference);


//============================
console.log("Test function declarations");
//============================

function square(x) {
    return x * x;
}

console.log(square(21));
console.log(square(4 + 2));
console.log(square(3) + square(4));


//============================
console.log("Test compound functions");
//============================

console.log(square(square(3)));

function sum_of_squares(x, y) {
    return square(x) + square(y);
}

function f(a) {
    return( sum_of_squares(a + 1, a * 2));
}

console.log(f(5));


//===============================
console.log("Test booleans and conditionals");
//===============================

console.log((3 > 2) && (5 < 9));
console.log((3 > 2) && (5 > 9));
console.log((3 > 2) || (5 > 9));
console.log((3 < 2) || (5 < 9));

function abs(x) {
    return x >= 0 ? x : -x;
}

console.log(abs(6));
console.log(abs(-5));
console.log(abs(0));

function abs2(x) {
    return x > 0
	       ? x
		   : x === 0
		   ? 0
		   : -x;
}

console.log(abs2(10));
console.log(abs2(-33));
console.log(abs2(0));

function close_enough(x, y) {
    return abs(x - y) < 0.001;
}

console.log(close_enough(12.0003, 12));
console.log(close_enough(12.1, 12));


//==========================
console.log("Test recursive function");
//==========================

function factorial(n) {
    return n === 1
	       ? 1
		   : n * factorial(n - 1);
}

console.log(factorial(4));
console.log(factorial(8));
		   

function fib(n) {
    return n === 0
	       ? 0
		   : n === 1
		   ? 1
		   : fib(n - 1) + fib(n - 2);
}

console.log(fib(4));
console.log(fib(8));


ENDOFTEXT
))

