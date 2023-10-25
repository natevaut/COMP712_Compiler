#lang eopl

(define lang-lexical-spec
  '(
    ;(token-name (what-to-match) data-type)
    ;arbno = arbitrary number of ...

    ;Lexical elements
    ;order matters (top-down eval)
    
    (whitespace (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (keyword (
       (or "const" "function")
      ) symbol)
    (identifier (
       (or letter "_" "$")
       (arbno (or letter digit "_" "$"))
     ) symbol)
    (punctuation ("=") string)
    (number (digit (arbno digit)) number)
  )
)


(define lang-grammar
  '(
    ;Grammar parts
    
    ;defaults below 
    (program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (primitive ("+") add-prim)
    (primitive ("-") subtract-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
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

(define stmt1 "const x = 1 // comment")

(display (just-scan stmt1))