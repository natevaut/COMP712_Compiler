#lang eopl
;(require "./Parser.rkt")
(require "./Structures.rkt")

;(provide value-of-program value-of)

;;;;;;;;;;;
(define basic-lex '(
    (whitespace (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (number ((arbno digit)) number)
))

(define basic-grmr '(
    (program (expression) a-program)
    (expression (math-expression) a-math-expr)
    (math-expression (math-term math-expression+) an-expr)
    (math-expression+ ("+" math-term math-expression+) an-add-expr)
    (math-expression+ ("-" math-term math-expression+) a-sub-expr)
    (math-expression+ () null-expr)
    (math-term (math-factor math-term+) a-factor)
    (math-term+ ("*" math-factor math-term+) a-mult-term)
    (math-term+ ("/" math-factor math-term+) a-div-term)
    (math-term+ () null-term)
    (math-factor (number) a-number)
    (math-factor ("(" expression ")") a-group)
))

(sllgen:make-define-datatypes basic-lex basic-grmr)
;;;;;;;;;

;(define init-env empty-env)


(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (expr) (value-of-expr expr) ]
    )
  )
)
(define value-of-expr
  (lambda (exp)
    (cases expression exp
      [a-math-expr (expr) (value-of-math-expr expr)])))

(define value-of-math-expr
  (lambda (exp)
    (cases math-expression exp
      [an-expr (x y) (value-of-math-expr2 x y)])))

(define value-of-math-expr2
  (lambda (first-term math-op)
    (cases math-expression+ math-op
      [an-add-expr (t e+) (+ (value-of-math-term first-term)
                             (value-of-math-expr2 t e+))]
      [a-sub-expr (t e+) (- (value-of-math-term first-term)
                            (value-of-math-term t))] ; Fix subtraction here
      [null-expr () (value-of-math-term first-term)])))


(define value-of-math-term
  (lambda (tm)
    (cases math-term tm
      [a-factor (f t+) (value-of-math-term2 f t+)])))

(define value-of-math-term2
  (lambda (fac t+)
    (cases math-term+ t+
      [a-mult-term (f t+) (* (value-of-math-term2 f t+)
                             (value-of-math-factor fac))]
      [a-div-term (f t+) (/ (value-of-math-factor fac)
                            (value-of-math-term2 f t+))]
      [null-term () (value-of-math-factor fac)])))

(define value-of-math-factor
  (lambda (f)
    (cases math-factor f
      (a-number (x) x)
      (a-group (exp) (value-of-expr exp)))))

;


(define scan+parse
    (sllgen:make-string-parser basic-lex basic-grmr))

(define (run s)
    (value-of-program (scan+parse s)))

(define REPL
  (sllgen:make-rep-loop "-->"
                        (lambda (pgm) (value-of-program pgm))
                        (sllgen:make-stream-parser basic-lex basic-grmr)))

(REPL)