#lang eopl
;(require "./Parser.rkt")
(require "./Structures.rkt")

;(provide value-of-program value-of)

;;;;;;;;;;;
(define basic-lex '(
    (whitespace (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    
    (number ((arbno digit)) number)
    (number ((arbno digit) "." digit (arbno digit)) number)
    (boolean ((or "true" "false")) symbol)
    (null ("null") symbol)
    (terminal (";") symbol)
    (quoted-string ("\"" (arbno (not #\")) "\"") string)
    (quoted-string ("\'" (arbno (not #\')) "\'") string)
    (quoted-string ("`" (arbno (not #\`)) "`") string)
    (identifier (
       (or letter "_" "$")
       (arbno (or letter digit "_" "$"))
     ) symbol)
))

(define basic-grmr '(
    (program (statements) a-program)
    (statements (expression terminal statements+) some-statements)
    (statements ("if" "(" expression ")" "{" statements "}" else-content) if-block)
    (else-content ("else" "{" statements "}") else-block)
    (else-content () else-block-empty)
    (statements ("const" identifier "=" expression terminal) a-const-decl)
    (statements+ () empty-statements+)
    (statements+ (statements) some-statements+)
    (expression (bin-operation) a-bin-op-expr)
    (expression (boolean) a-boolean)
    (expression (quoted-string) a-string)
    (expression (null) null)
    (bin-operation (math-expression bin-operation+) a-bin-op)
    (bin-operation+ ("===" math-expression bin-operation+) an-equality-op)
    (bin-operation+ ("!==" math-expression bin-operation+) an-inequality-op)
    (bin-operation+ ("<" math-expression bin-operation+) a-lt-op)
    (bin-operation+ ("<=" math-expression bin-operation+) a-lte-op)
    (bin-operation+ (">" math-expression bin-operation+) a-gt-op)
    (bin-operation+ (">=" math-expression bin-operation+) a-gte-op)
    (bin-operation+ ("&&" math-expression bin-operation+) an-and-op)
    (bin-operation+ ("||" math-expression bin-operation+) an-or-op)
    (bin-operation+ () null-math-op)
    (math-expression (math-term math-expression+) an-expr)
    (math-expression+ ("%"  math-term math-expression+) a-mod-expr)
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
      [a-program (sts) (value-of-sts sts) ]
    )
  )
)

(define value-of-st+
  (lambda (st+)
    (cases statements+ st+
      [empty-statements+ () 'empty]
      [some-statements+ (sts) (value-of-sts sts)]
    )
  )
)

(define value-of-sts
  (lambda (sts)
    (cases statements sts
      [some-statements (expr _ st+)
        ; Run both statements
        ; Return the second if not empty else first
        (define expr-ran (value-of-expr expr))
        (define sts-ran (value-of-st+ st+))
        (if (eq? sts-ran 'empty)
            expr-ran
            sts-ran
        )
      ]
      [if-block (test if-sts else-block-content)
        ; To parse the else block
        (define value-of-else-content (lambda (content)
           (cases else-content content
             [else-block (sts) (value-of-sts sts)]
             [else-block-empty () 'undefined]
           )
        ))
        ; Parse the if statement
        (if (truthy (value-of-expr test))
          (value-of-sts if-sts)
          (value-of-else-content else-block-content)
        )
      ]
      [a-const-decl (id st _) (set! id st)]
    )
  )
)

(define value-of-expr
  (lambda (exp)
    (cases expression exp
      [a-bin-op-expr (op) (value-of-bin-op op)]
      [a-boolean (bool) bool]
      [a-string (str) str]
      [null (null) 'null]
    )
  )
)

(define value-of-bin-op
  (lambda (op)
    (cases bin-operation op
      [a-bin-op (expr op+) (value-of-bin-op2 expr op+)]
    )
  )
)

(define value-of-bin-op2
  (lambda (first-expr rest-op+)
    (cases bin-operation+ rest-op+
      [an-equality-op (expr op+)
        (eq? (value-of-math-expr first-expr) (value-of-bin-op2 expr op+))
      ]
      [an-inequality-op (expr op+)
        (not (eq? (value-of-math-expr first-expr) (value-of-bin-op2 expr op+)))
      ]
      [a-lt-op (expr op+)
        (< (value-of-math-expr first-expr) (value-of-bin-op2 expr op+))
      ]
      [a-lte-op (expr op+)
        (<= (value-of-math-expr first-expr) (value-of-bin-op2 expr op+))
      ]
      [a-gt-op (expr op+)
        (> (value-of-math-expr first-expr) (value-of-bin-op2 expr op+))
      ]
      [a-gte-op (expr op+)
        (>= (value-of-math-expr first-expr) (value-of-bin-op2 expr op+))
      ]
      [an-and-op (expr op+)
        (and (value-of-math-expr first-expr) (value-of-bin-op2 expr op+))
      ]
      [an-or-op (expr op+)
        (or (value-of-math-expr first-expr) (value-of-bin-op2 expr op+))
      ]
      [null-math-op () (value-of-math-expr first-expr)]
    )
  )
)

(define value-of-math-expr
  (lambda (exp)
    (cases math-expression exp
      [an-expr (t e+) (value-of-math-expr2 t e+)]
    )
  )
)

(define value-of-math-expr2
  (lambda (first-term expr+)
    (cases math-expression+ expr+
      [an-add-expr (t e+) (+ (value-of-math-term first-term)
                             (value-of-math-expr2 t e+))]
      [a-sub-expr (t e+) (- (value-of-math-term first-term)
                            (value-of-math-expr2 t e+))]
      [a-mod-expr (t e+) (modulo (value-of-math-term first-term)
                                 (value-of-math-expr2 t e+))]
      [null-expr () (value-of-math-term first-term)]
    )
  )
)


(define value-of-math-term
  (lambda (tm)
    (cases math-term tm
      [a-factor (f t+) (value-of-math-term2 f t+)]
    )
  )
)

(define value-of-math-term2
  (lambda (fac t+)
    (cases math-term+ t+
      [a-mult-term (f t+) (* (value-of-math-term2 f t+)
                             (value-of-math-factor fac))]
      [a-div-term (f t+) (/ (value-of-math-factor fac)
                            (value-of-math-term2 f t+))]
      [null-term () (value-of-math-factor fac)]
    )
  )
)

(define value-of-math-factor
  (lambda (f)
    (cases math-factor f
      [a-number (x) x]
      [a-group (exp) (value-of-expr exp)]
    )
  )
)

(define (truthy val) (not (falsey val)))
(define (falsey val)
  (or
    (eq? val 'false)
    (eq? val 'null)
    (eq? val 'undefined)
    (eq? val 0)
  )
)

; Run ;


(define scan+parse
    (sllgen:make-string-parser basic-lex basic-grmr))

(define (run s)
    (value-of-program (scan+parse s)))

(define REPL
  (sllgen:make-rep-loop "-->"
    (lambda (pgm) (value-of-program pgm))
    (sllgen:make-stream-parser basic-lex basic-grmr)
  )
)

(REPL)