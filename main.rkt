#lang racket

(require "parser.rkt")

(provide evaluate)

(define (evaluate file-path)
  (define in (open-input-file file-path))
  (define my-lexer (lex-this lexer-imp in))
  (let ((parser-res (pythonic-parser my-lexer))) (interpret-Program parser-res))
  (close-input-port in)
  )

; test
(evaluate "input.py")