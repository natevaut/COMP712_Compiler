#lang eopl
(require "./Project.rkt")

(display (scan+parse #<<ENDOFTEXT

const size = 5+4; size;

ENDOFTEXT
))