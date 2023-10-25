#lang eopl

(define lang-lexical-spec
  '(
    ;(token-name (what-to-match) data-type)
    ;arbno = arbitrary number of ...

    ;Lexical elements
    ;order matters (top-down eval)

    ; Ignored stuff
    (whitespace (whitespace) skip)
    (Comments ("//" (arbno (not #\newline))) skip)

    ;Keywords
    (const ("const") symbol)
    (function ("function") symbol)
    (return ("return") symbol)
    (if ("if") symbol)
    (else ("else") symbol)

    ;Expressions
    (number ((arbno digit)) number)
    (boolean ((or "true" "false")) string)
    (null ("null") string)
    (binary-logical ((or "&&" "||")) symbol)
    (unary-operator ((or "!" "-")) symbol)
    (binary-operator (
        (or "+" "-" "*" "/" "%" "===" "!==" ">" "<" ">=" "<=")
    ) symbol)

    ; Delimiters
    (equals ("=") symbol)
    (semi (";") symbol)
    
    ; Variables/user-defined
    (quoted-string ("\"" (arbno (not #\")) "\"") string)
    (identifier (
       (or letter "_" "$")
       (arbno (or letter digit "_" "$"))
     ) symbol)
  )
)


(define lang-grammar
  '(
    ;Grammar parts

    ;Program root
    (program (expression) Program_)

    ;Statements
   ; (statement
   ;     (expression binary-logical expression)
   ;     Binary-operator-expression)
   ; (name (identifier) Name-expression)
    
    ;defaults below
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (primitive ("+") add-prim)
  )
)

;; boilerplate

(sllgen:make-define-datatypes lang-lexical-spec lang-grammar)
  
(define show-lang-datatypes
    (lambda () (sllgen:list-define-datatypes lang-lexical-spec lang-grammar)))
  
(define scan&parse
    (sllgen:make-string-parser lang-lexical-spec lang-grammar))

(define just-scan
    (sllgen:make-string-scanner lang-lexical-spec lang-grammar))

(define stmt1 "const x = 1; true; // comment")

(display (just-scan stmt1))