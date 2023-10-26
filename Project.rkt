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
    (program (statements) Program)

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

    (statements (statement statements-rest) statements-lines)
    (statements-rest () statements-rest-empty)
    (statements-rest (statement statements-rest) statements-rest-some)
    
    ;names
        (names-rest () no-more-identifiers)
        (names-rest ("," names) more-identifiers)
    (names (identifier names-rest) identifiers-list-plain)

    ;Blocks
    (block ("{" statement "}") code-block)
     (if-header ("if" "(" expression ")") if-block-header)
     (if-block (if-header block) if-block-braced)
     (else-block ("else" block) else-block-braced)
     (else-block () else-block-empty)
     (else-if-block ("else" if-block) else-if-block-braced)
     (conditional-block (if-block else-block) if-else-block)
    
    ;Atomic expressions
    (atomic (number) number)
    (atomic (boolean) boolean)
    (atomic (quoted-string) string)
    (atomic (null) null)
    (atomic (identifier) name-expression)
    (atomic ("(" expressions ")") parenthetical-expressions)
    (atomic (unary-operator atomic) unary-expression)
    
        (binary-component (binary-operator) binary-operator-component)
        (binary-component (binary-logical) binary-logical-component)
    (tail (binary-component atomic) logical-expression-tail)
        (lambda-tail (block) lambda-tail-block)
        (lambda-tail (atomic) lambda-tail-expression)
    (tail ( "=>" lambda-tail) lambda-declaration-tail)
    (tail ("?" atomic ":" atomic) conditional-expression-tail)

    (expression (atomic expression-post) expression-item)
    (expression-post () expression-post-empty)
    (expression-post (tail expression-post) expression-post-rest)

        (expressions-rest () expressions-tail-empty)
        (expressions-rest ("," expressions) expressions-tail-many)
    (expressions () expressions-none)
    (expressions (expression expressions-rest) expressions-some)
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

(provide scan)
(provide scan+parse)
