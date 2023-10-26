#lang eopl
(require "./Project.rkt")

(display (scan+parse #<<ENDOFTEXT

function x(x) { return true; }

ENDOFTEXT
))