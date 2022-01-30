#lang racket


(require (lib "eopl.ss"  "eopl"))


; util

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define non-empty-list-of
    (lambda (predicate)
        (lambda (l)
            (and 
                (not (null? l)) 
                (predicate (first l) 
                ((list-of predicate) (rest l)))))))

; datatypes

(define-datatype Program Program?
    (a-program
        (statements (non-empty-list-of Statement?))))


(define-datatype Statement Statement?
    (a-compound-statement
        (compound-statement Compound-Statement?))
    (a-simple-statement
        (simple-statement Simple-Statement?))
    )


(define-datetype Simple-Statement Simple-Statement?
    (an-assignment
        (assignment Assignment?))
    (a-return-statement
        (return-statement Return-Statement))
    (a-pass-statement)
        ()
    )

(define-datetype Compound-Statement Compound-Statement?
    (a-function-definition
        (function-definition Function-Definition?))
    (a-if-statement
        (if-statement If-Statement?))
;    (a-for-statement
;        (for-statement For-Statement?))
    )

#|

(define-datetype Function-Definition Function-Definition?
    (function
        (id symbol?)
        (params (list-of Param?))
        (statements (list-of Statement?))))

(define-datatype )


|#
