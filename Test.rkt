#lang eopl
(require "./Parser.rkt")
(require "./Interpreter.rkt")

(define code #<<ENDOFTEXT

function x(x) { return true; }
console.log(1, 2);
x(2);

ENDOFTEXT
)

(display (run code))
