#lang eopl
(require "./Project.rkt")

(display (scan+parse #<<ENDOFTEXT

square(4+2);

ENDOFTEXT
))