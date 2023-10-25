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

    ; Punctuation
    (equals ("=") symbol)
    (semi (";") symbol)
    (lparen ("(") symbol)
    (rparen (")") symbol)
    (lbrace ("{") symbol)
    (rbrace ("}") symbol)
    
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
    (program (statement) Program)

    ;Blocks
    (block (lbrace statement rbrace) code-block)
    (if-block (if lparen expression lparen block) if-block-braced)
    ;(if-block (if "(" statement ")" statement) if-block-unbraced)
    (else-block (else block) else-block-braced)
    ;(else-block (else expression) else-block-unbraced)
    ;(else-block (else if-block) else-if-block)
    (conditional-statement (if-block) condition-if)
    ;(conditional-statement (if-block else-block) condition-if-else)

    ; Statements
    (statement (expression semi) simple-statement)
    
    ;Expressions
    ;(expression (identifier) name)
    ;(expression (expression binary-operator expression)
    ;   binary-expression)
    ;(statement (unary-operator expression)
    ;    Unary-operator-combination)

    ;Expressions
    (expression (number) literal)

    ;Primitives
    (primitive (number) primitive-number)
    (primitive (boolean) primitive-boolean)
    (primitive (quoted-string) primitive-string)
    (primitive (null) primitive-list)
    (primitive (binary-logical) primitive-logical)
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

(define stmt1 "1;")

(display (scan&parse stmt1))