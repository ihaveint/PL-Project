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
    (program
        (statements (non-empty-list-of Statement?))))


(define-datatype Statement Statement?
    (compound-statement-statement
        (compound-statement Compound-Statement?))
    (simple-statement-statement
        (simple-statement Simple-Statement?))
    )

(define-datatype Simple-Statement Simple-Statement?
    (assignment-simple-statement
        (assignment Assignment?))
    (return-simple-statement
        (return-statement Return-Statement?))
    (pass-simple-statement))

(define-datatype Compound-Statement Compound-Statement?
;    (a-function-definition
;        (function-definition Function-Definition?))
    (an-if-statement
        (if-statement If-Statement?))
;    (a-for-statement
;        (for-statement For-Statement?))
    )


(define-datatype Assignment Assignment?
    (assignment
        (id symbol?)
        (expression Expression?))
    )

(define-datatype Return-Statement Return-Statement?
	(simple-return)
	(expression-return
		(expression Expression?)))
#|

(define-datetype Function-Definition Function-Definition?
    (function
        (id symbol?)
        (params (list-of Param?))
        (statements (list-of Statement?))))

(define-datatype )


|#



(define-datatype If-Statement If-Statement?
	(if-statement
		(if-expression Expression?)
		(if-body (non-empty-list-of Statement?))
		(else-body Else-Block?)))

(define-datatype Else-Block Else-Block?
	(else-block
		(else-statements (non-empty-list-of Statement?))))


(define-datatype Expression Expression?
	(disjunction-expression
		(disjunction Disjunction?))
	(sum-expression
		(sum Sum?)))

(define-datatype Disjunction Disjunction?
	(conjunction-expression
		(conjunction Conjunction?))
	(or-expression
		(disjunction Disjunction?)
		(conjunction Conjunction?)))
	

(define-datatype Conjunction Conjunction?
	(inversion-expression
		(inversion Inversion?))
	(and-expression
		(conjunction Conjunction?)
		(inversion Inversion?)))

(define-datatype Inversion Inversion?
	(simple-inversion
		(inversion Inversion?))
	(comparison-expression
		(comparison Comparison?)))


(define-datatype Comparison Comparison?
	(eq-sum
		(left-sum Sum?)
		(right-sum Sum?))
	(lt-sum
		(left-sum Sum?)
		(right-sum Sum?))
	(gt-sum
		(left-sum Sum?)
		(right-sum Sum?)))
	
(define-datatype Sum Sum?
	(addition
		(sum Sum?)
		(term Sum?))
	(subtraction
		(sum Sum?)
		(term Term?))
	(term-sum
		(term Term?)))


(define-datatype Term Term?
	(muliplication
		(term Term?)
		(factor Factor?))
	(division
		(term Term?)
		(factor Factor?))
	(factor-term
		(factor Factor?)))


(define-datatype Factor Factor?
	(plus
		(factor Factor?))
	(minus
		(factor Factor?))
;	(power-factor
;		(power Power?))
		)
		

