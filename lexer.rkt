#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens value-tokens (NUM))
(define-tokens v (ID))
(define-empty-tokens op-tokens (= + - * / or and : \; == > < if else for in ** \[ \] \( \) True False None def EOF))
(define-lex-abbrevs
 (digit (:/ "0" "9"))
 (char (:or (:/ "a" "z") (:/ "A" "Z") (:/ "0" "9"))))

(define simple-math-lexer
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space #\newline)
    (simple-math-lexer input-port)]
   [(:: (:+ digit) (:? ".") (:* digit))
    (token-NUM (string->number lexeme))]
   [(:or "=" "+" "-" "*" "/" "*" "or" "and" ":" ";" "==" ">" "<" "if" "else" "for" "in" "**" "[" "]" "(" ")" "True" "False" "None" "def")
    (string->symbol lexeme)]
   [(:: (:+ char)) (token-ID (string->symbol lexeme))]
   ))



;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "0.1+2+ 356 + otr else if for in 4 or yt")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
