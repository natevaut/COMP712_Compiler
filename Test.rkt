#lang eopl
(require "./Parser.rkt")

(display (scan+parse #<<ENDOFTEXT

function x(x) { return true; }

ENDOFTEXT
))