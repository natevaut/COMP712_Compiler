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

    ;Expressions
    (number ((arbno digit)) number)
    (boolean ((or "true" "false")) string)
    (null ("null") string)
    (binary-logical ((or "&&" "||")) symbol)
    (unary-operator ((or "!" "-")) symbol)
    (binary-operator (
        (or "+" "-" "*" "/" "%" "===" "!==" ">" "<" ">=" "<=")
    ) symbol)
    
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

    ; Statements (a line of code)
    (statement ("const" identifier "=" expression ";")
       const-declaration)
    (statement ("function" identifier "(" names ")" block)
       function-declaration)
    (statement ("return" expression ";")
       return-statement)
    (statement (conditional-block) conditional-statement)
    (statement (block) block-statement)
    (statement (expression ";") expression-statement)
    
    ;names
    (names (identifier) one-name)
    ;TODO many names

    ;Blocks
    (block ("{" statement "}") code-block)
     (if-header ("if" "(" expression "(") if-block-header)
     (if-block (if-header block) if-block-braced)
     (else-block ("else" block) else-block-braced)
     (else-block () else-block-empty)
     (else-if-block ("else" if-block) else-if-block-braced)
     (conditional-block (if-block else-block) if-else-block)
    
    ;Atomic expressions
    (expression (number) number)
    (expression (boolean) boolean)
    (expression (quoted-string) string)
    (expression (null) null)
    (expression (identifier) name-expression)
    ;TODO bin operator combin
    (expression (unary-operator expression)
       unary-operator-combination)
    ;TODO logical composition
    ;TODO function application
    ;TODO lambda expression
    ;TODO lambda expression-2
    ;TODO conditional expression ?:
    (expression ("(" expression ")")
       parenthetical-expression)

    ;expressions
    ;TODO expressions
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

(define stmt1 "-1;")

(display (scan&parse stmt1))