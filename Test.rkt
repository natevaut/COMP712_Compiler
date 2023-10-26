#lang eopl
(require "./Project.rkt")

(display (scan+parse #<<ENDOFTEXT

sqyare(3) + square (4)

ENDOFTEXT
))