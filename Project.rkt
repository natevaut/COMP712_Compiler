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
    (quoted-string ("\'" (arbno (not #\')) "\'") string)
    (quoted-string ("`" (arbno (not #\`)) "`") string)
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

    ;terminals
    (terminal (";") terminal-semi)
    (terminal () terminal-implied)

    ; Statements (a line of code)
    (statement ("const" identifier "=" expression terminal)
       const-declaration)
    (statement ("function" identifier "(" names ")" block)
       function-declaration)
    (statement ("return" expression terminal)
       return-statement)
    (statement (conditional-block) conditional-statement)
    (statement (block) block-statement)
    (statement (expression terminal) expression-statement)
    
    ;names
        (names-rest () no-more-identifiers)
        (names-rest ("," names) more-identifiers)
    (names (identifier names-rest) identifiers-list-plain)

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
        (binary-component (binary-operator) binary-operator-component)
        (binary-component (binary-logical) binary-logical-component)
    (expression ("%=%" expression binary-component expression) logical-expression)
    (expression (unary-operator expression) unary-expression)
    ;TODO logical composition
    (expression ("%()%" identifier "(" expressions ")") function-call)
        (lambda-tail (block) lambda-tail-block)
        (lambda-tail (expression) lambda-tail-expression)
    (expression ("%=>%" "(" names ")" "=>" lambda-tail) lambda-declaration)
    (expression ("%?:%" expression "?" expression ":" expression) conditional-expression)
    (expression ("(" expression ")") parenthetical-expression)

        (expressions-tail () expressions-tail-empty)
        (expressions-tail ("," expressions) expressions-tail-many)
    (expressions () expressions-none)
    (expressions (expression expressions-tail) expressions-some)
  )
)

;; boilerplate

(sllgen:make-define-datatypes lang-lexical-spec lang-grammar)
  
(define show-lang-datatypes
    (lambda () (sllgen:list-define-datatypes lang-lexical-spec lang-grammar)))
  
(define scan+parse
    (sllgen:make-string-parser lang-lexical-spec lang-grammar))

(define scan
    (sllgen:make-string-scanner lang-lexical-spec lang-grammar))

(define stmt1 "return %?:% %=% a + b ? c : d;")

(display (scan+parse stmt1))