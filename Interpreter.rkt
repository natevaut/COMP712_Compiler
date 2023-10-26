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
    (expression (term expression+) an-expr)
    (expression+ ("+" term expression+) an-add-expr)
    (expression+ ("-" term expression+) a-sub-expr)
    (expression+ () null-expr)
    (term (factor term+) a-factor)
    (term+ ("*" factor term+) a-mult-term)
    (term+ ("/" factor term+) a-div-term)
    (term+ () null-term)
    (factor (number) a-number)
    (factor ("(" expression ")") a-group)
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
      [an-expr (x y) (value-of-expr2 x y)])))

(define value-of-expr2
  (lambda (first-term math-op)
    (cases expression+ math-op
      [an-add-expr (t e+) (+ (value-of-term first-term)
                             (value-of-expr2 t e+))]
      [a-sub-expr (t e+) (- (value-of-term first-term)
                            (value-of-term t))] ; Fix subtraction here
      [null-expr () (value-of-term first-term)])))


(define value-of-term
  (lambda (tm)
    (cases term tm
      [a-factor (f t+) (value-of-term2 f t+)])))

(define value-of-term2
  (lambda (fac t+)
    (cases term+ t+
      [a-mult-term (f t+) (* (value-of-term2 f t+)
                             (value-of-factor fac))]
      [a-div-term (f t+) (/ (value-of-factor fac)
                            (value-of-term2 f t+))]
      [null-term () (value-of-factor fac)])))

(define value-of-factor
  (lambda (f)
    (cases factor f
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