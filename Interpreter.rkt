#lang eopl

;(provide value-of-program value-of)

; Environment

(define-datatype environment environment?
  (empty-env)
  (extended-env
   (sym symbol?)
   (val anything?)
   (env environment?)))

(define anything? (lambda (v) #t))

(define (lookup-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'lookup-env "undefined variable '~s'" search-sym))
    (extended-env (sym val old-env)
                  (if (eqv? sym search-sym)
                      val
                      (lookup-env old-env search-sym)))))

(define (set-value env id val)
  (set! global-env (extended-env id val global-env))
  'undefined
)
(define (get-value env id)
  (lookup-env global-env id)
)

(define global-env (empty-env))

(set-value global-env 'return (lambda (val) val))

;;;;;;;;;;;
(define basic-lex '(
    (whitespace (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (comment ("/*" (arbno (not #\null)) "*/") skip)
    
    (number ((arbno digit)) number)
    (number ((arbno digit) "." digit (arbno digit)) number)
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
    (block ("{" statements "}") braced-block)
    (block (expression terminal) unbraced-block)
    (func-statements (func-statement func-statement+) func-sts)
    (func-statement+ (func-statement) func-st+)
    (func-statement+ () func-st+-empty)
    (func-statement (statement) func-st)
    (func-statement ("return " expression terminal) func-return)
    (func-statement (#<<ENDOFRETURN
 return
ENDOFRETURN
                     expression terminal) func-return-undefined)
    (statements (statement statements+) some-statements)
    (statement (terminal) empty-st)
    (statement (expression terminal) expr-statement)
    (statement ("if" "(" expression ")" block else-content) if-block)
    (else-content ("else" block) else-block)
    (else-content () else-block-empty)
    (statement ("const" identifier "=" expression terminal) a-const-decl)
    (statement ("function" identifier "(" (separated-list identifier ",") ")" "{" func-statements "}") function-decl)
    (statements+ () empty-statements+)
    (statements+ (statements) some-statements+)
    (expression (bin-operation expression+) a-bin-op-expr)
    (expression ("console" "." "log" "(" (separated-list expression ",") ")") console.log)
    (expression+ ("?" expression ":" expression) ternary-expr+)
    (expression+ () empty-expr+)
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
    (math-term (atomic math-term+) a-factor)
    (math-term+ ("*" atomic math-term+) a-mult-term)
    (math-term+ ("/" atomic math-term+) a-div-term)
    (math-term+ () null-term)
    (identifier-post () just-identifier)
    (identifier-post ("(" (separated-list expression ",") ")") func-call)
    (atomic (identifier identifier-post) an-identifier)
    (atomic (number) a-number)
    (atomic (quoted-string) a-string)
    (atomic ("true") true)
    (atomic ("false") false)
    (atomic ("null") null)
    (atomic ("undefined") undefined)
    (atomic ("-" atomic) unary-minus)
    (atomic ("!" atomic) unary-not)
    (atomic ("(" (separated-list expression ",") ")" lambda-part) a-group)
    (lambda-part ("=>" "{" func-statements "}") lambda-part-braced)
    (lambda-part () no-lambda-part)
))

(sllgen:make-define-datatypes basic-lex basic-grmr)
;;;;;;;;;

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (sts) (value-of-sts sts global-env)]
    )
  )
)

(define value-of-block
  (lambda (blk env)
    (cases block blk
      [braced-block (sts) (value-of-sts sts env)]
      [unbraced-block (expr _) (value-of-expr expr env)]
    )
  )
)

(define value-of-st+
  (lambda (st+ env)
    (cases statements+ st+
      [empty-statements+ () 'empty]
      [some-statements+ (sts) (value-of-sts sts env)]
    )
  )
)

(define value-of-func-st
  (lambda (st env)
    (cases func-statement st
      [func-st (st) (value-of-st st env)]
      [func-return (expr _) (value-of-expr expr env)]
      [func-return-undefined (expr _) 'empty]
    )
  )
)

(define value-of-func-sts
  (lambda (sts params+args env)
    ; list of (param . val)
    (define param-arg-list (apply map list params+args))
    (define (apply-arg-val pair)
      (set-value env (car pair) (cadr pair))
    )
    (map apply-arg-val param-arg-list)
    
    (cases func-statements sts
      [func-sts (st sts+) (value-of-func-sts+ st sts+ env)]
    )
  )
)

(define value-of-func-sts+
  (lambda (st sts+ env)
    (cases func-statement+ sts+
      [func-st+ (st) (value-of-func-sts+ st sts+ env)]
      [func-st+-empty () (value-of-func-st st env)]
    )
  )
)

(define value-of-sts
  (lambda (sts env)
    (cases statements sts
      [some-statements (st st+)
        ; Run both statements
        ; Return the second if not empty else first
        (define st-ran (value-of-st st env))
        (define sts-ran (value-of-st+ st+ env))
        (if (eq? sts-ran 'empty)
            st-ran
            sts-ran
        )
      ]
    )
  )
)

(define value-of-st
  (lambda (st env)
    (define (value-of-expr-w/-env expr) (value-of-expr expr env))
    (cases statement st
      [expr-statement (expr _) (value-of-expr expr env)]
      [if-block (test if-blk else-blk)
        ; To parse the else block
        (define value-of-else-content (lambda (content)
           (cases else-content content
             [else-block (blk) (value-of-block blk env)]
             [else-block-empty () 'undefined]
           )
        ))
        ; Parse the if statement
        (if (truthy (value-of-expr test env))
          (value-of-block if-blk env)
          (value-of-else-content else-blk env)
        )
      ]
      [a-const-decl (id decl-expr _) (set-value env id (value-of-expr decl-expr env))]
      [function-decl (id params func-sts) (set-value env id
                       (lambda args (value-of-func-sts func-sts (cons params args) env)))]
      [empty-st (_) 'empty]
    )
  )
)

(define value-of-expr
  (lambda (exp env)
    (define (value-of-expr-w/-env expr) (value-of-expr expr env))
    (cases expression exp
      [a-bin-op-expr (op e+) (value-of-expr2 op e+ env)]
      [console.log (exprs) (display (map value-of-expr-w/-env exprs)) (display #\newline)]
    )
  )
)

(define value-of-expr2
  (lambda (op expr+ env)
    (define op-val (value-of-bin-op op env))
    (cases expression+ expr+
      [ternary-expr+ (e1 e2) (if (truthy op-val) (value-of-expr e1 env) (value-of-expr e2 env))]
      [empty-expr+ () op-val]
    )
  )
)

(define value-of-bin-op
  (lambda (op env)
    (cases bin-operation op
      [a-bin-op (expr op+) (value-of-bin-op2 expr op+ env)]
    )
  )
)

(define value-of-bin-op2
  (lambda (first-expr rest-op+ env)
    (cases bin-operation+ rest-op+
      [an-equality-op (expr op+)
        (eq? (value-of-math-expr first-expr env) (value-of-bin-op2 expr op+ env))
      ]
      [an-inequality-op (expr op+)
        (not (eq? (value-of-math-expr first-expr env) (value-of-bin-op2 expr op+ env)))
      ]
      [a-lt-op (expr op+)
        (< (value-of-math-expr first-expr env) (value-of-bin-op2 expr op+ env))
      ]
      [a-lte-op (expr op+)
        (<= (value-of-math-expr first-expr env) (value-of-bin-op2 expr op+ env))
      ]
      [a-gt-op (expr op+)
        (> (value-of-math-expr first-expr env) (value-of-bin-op2 expr op+ env))
      ]
      [a-gte-op (expr op+)
        (>= (value-of-math-expr first-expr env) (value-of-bin-op2 expr op+ env))
      ]
      [an-and-op (expr op+)
        (define first (value-of-math-expr first-expr env))
        (define rest (value-of-bin-op2 expr op+ env))
        (if (and (truthy first) (truthy rest)) rest #f)
      ]
      [an-or-op (expr op+)
        (define first (value-of-math-expr first-expr env))
        (define rest (value-of-bin-op2 expr op+ env))
        (if (truthy first) first rest)
      ]
      [null-math-op () (value-of-math-expr first-expr env)]
    )
  )
)

(define value-of-math-expr
  (lambda (exp env)
    (cases math-expression exp
      [an-expr (t e+) (value-of-math-expr2 t e+ env)]
    )
  )
)

(define value-of-math-expr2
  (lambda (first-term expr+ env)
    ;first-term may be num
    (define first (value-of-math-term first-term env))
    (define plus-func (if (number? first) + quoted-string-append))
    (cases math-expression+ expr+
      [an-add-expr (t e+) (plus-func first (value-of-math-expr2 t e+ env))]
      [a-sub-expr (t e+) (value-of-math-expr2 (- first (value-of-math-term t env)) e+ env)]
      [a-mod-expr (t e+) (modulo first (value-of-math-expr2 t e+ env))]
      [null-expr () first]
    )
  )
)


(define value-of-math-term
  (lambda (tm env)
   ; ! tm can be number
   (if (number? tm)
    tm
    (cases math-term tm
      [a-factor (f t+) (value-of-math-term2 f t+ env)]
    )
   )
  )
)

(define value-of-math-term2
  (lambda (fac t+ env)
    ; ! fac can be num
    (cases math-term+ t+
      [a-mult-term (f t+) (* (value-of-math-term2 f t+ env) (value-of-atomic fac env))]
      ; Divide: avoid doing improper order of operations of 'a / (b ...)' (correct: '(a / b) ...')
      [a-div-term (f t+) (value-of-math-term2
                           (/ (value-of-atomic fac env) (value-of-atomic f env)) t+ env)]
      [null-term () (value-of-atomic fac env)]
    )
  )
)

(define value-of-identifier
  (lambda (id post env)
    (define val (get-value env id))
    (define (value-of-expr-w/-env expr) (value-of-expr expr env))
    (cases identifier-post post
      [just-identifier () val]
      [func-call (params) (func-val (val (map value-of-expr-w/-env params)))]
    )
  )
)

(define (func-val pair-or-void)
  (if (pair? pair-or-void)
      (car pair-or-void)
      pair-or-void
  )
)

(define value-of-parenth-expr
  (lambda (exprs lbda-part env)
   (define (value-of-expr-w/-env expr) (value-of-expr expr env))
    (cases lambda-part lbda-part
      [lambda-part-braced (sts)
         (define params (map value-of-expr-w/-env exprs))
         (lambda args (value-of-func-sts sts (cons params args) env))]
      [no-lambda-part () (car (reverse (map value-of-expr-w/-env exprs)))]
    )
  )
)

(define value-of-atomic
  (lambda (f env)
   ; f could be num
   (if (number? f)
    f
    (cases atomic f
      [a-number (x) x]
      [a-string (str) str]
      [an-identifier (id post) (value-of-identifier id post env)]
      [a-group (exprs lbda-part) (value-of-parenth-expr exprs lbda-part env)]
      [unary-minus (n) (- (value-of-atomic n env))]
      [unary-not (n) (not (value-of-atomic n env))]
      [true () #t]
      [false () #f]
      [undefined () 'undefined]
      [null () 'null]
    )
   )
  )
)

(define (truthy val) (not (falsey val)))
(define (falsey val)
  (or
    (eq? val #f)
    (eq? val 'null)
    (eq? val 'undefined)
    (eq? val 0)
  )
)

; "a" + "b" -> "ab"
(define (quoted-string-append a b)
  (define a-data (substring a 1 (- (string-length a) 1)))
  (define b-data (substring b 1 (- (string-length b) 1)))
  (string-append "\"" a-data b-data "\"")
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

(provide run)
(provide REPL)
(provide scan+parse)
