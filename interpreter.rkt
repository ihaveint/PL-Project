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
        (statements Statements?)))

(define-datatype Statements Statements?
	(single-statement
		(statement Statement?))
	(multiple-statements
		(statements Statements?)
		(statement Statement?)))


(define-datatype Statement Statement?
    (compound-statement-statement
        (compound-statement Compound-Statement?))
    (simple-statement-statement
        (simple-statement Simple-Statement?)))

(define-datatype Simple-Statement Simple-Statement?
    (assignment-simple-statement
        (assignment Assignment?))
    (return-simple-statement
        (return-statement Return-Statement?))
    (pass-simple-statement))

(define-datatype Compound-Statement Compound-Statement?
    (function-definition-compound-statement
        (function-definition Function-Definition?))
    (if-compound-statment
        (if-statement If-Statement?))
    (for-statement-compound-statement
        (for-statement For-Statement?)))

(define-datatype Assignment Assignment?
    (assignment
        (id symbol?)
        (expression Expression?)))

(define-datatype Return-Statement Return-Statement?
	(simple-return)
	(expression-return
		(expression Expression?)))

(define-datatype Function-Definition Function-Definition?
	(function-with-params
		(id symbol?)
		(params Params?)
		(statements Statements?))
	(function-with-no-param
			(id symbol?)
			(statements Statements?)))

(define-datatype Params Params?
	(single-param
		(param Param?))
	(multiple-params
		(params Params?)
		(param Param?)))

(define-datatype Param Param?
	(param
		(id symbol?)
		(default-value Expression?)))

(define-datatype If-Statement If-Statement?
	(if-statement
		(if-expression Expression?)
		(if-body (non-empty-list-of Statement?))
		(else-body Else-Block?)))

(define-datatype Else-Block Else-Block?
	(else-block
		(else-statements (non-empty-list-of Statement?))))

(define-datatype For-Statement For-Statement?
	(for
		(id symbol?)
		(expression Expression?)
		(statements Statements?)))



(define-datatype Expression Expression?
	(disjunction-expression
		(disjunction Disjunction?))
	(sum-expression
		(sum Sum?)))

(define-datatype Disjunction Disjunction?
	(conjunction-disjunction
		(conjunction Conjunction?))
	(or-disjunction
		(disjunction Disjunction?)
		(conjunction Conjunction?)))
	

(define-datatype Conjunction Conjunction?
	(inversion-conjunction
		(inversion Inversion?))
	(and-conjunction
		(conjunction Conjunction?)
		(inversion Inversion?)))

(define-datatype Inversion Inversion?
	(not-inversion
		(inversion Inversion?))
	(comparison-inversion
		(comparison Comparison?)))


(define-datatype Comparison Comparison?
	(eq-sum-comparison
		(eq-sum Eq-Sum?)
	(lt-sum-comparison
		(lt-sum Lt-Sum?))
	(gt-sum-comparison
		(gt-sum Gt-Sum?))))


(define-datatype Eq-Sum Eq-Sum?
	(eq-sum
		(left-sum Sum?)
		(right-sum Sum?)))


(define-datatype Lt-Sum Lt-Sum?
	(lt-sum
		(left-sum Sum?)
		(right-sum Sum?)))

(define-datatype Gt-Sum Gt-Sum?
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
	(power-factor
		(power Power?)))


(define-datatype Power Power?
	(power
		(atom Atom?)
		(factor Factor?))
	(primary-power
		(primary Primary?)))

(define-datatype primary Primary?
	(atom-primary
		(atom Atom?))
	(array-ref
		(primary Primary?)
		(expression Expression?))
	(simple-call
		(primary Primary?))
	(argument-call
		(primary Primary?)
		(arguments Arguments?)))

(define-datatype Arguments Arguments?
	(single-argument
		(expression Expression?))
	(multiple-arguments
		(arguments Arguments?)
		(expression Expression?)))

(define-datatype Atom Atom?	
	(id-atom	
		(id symbol?))
	(true-atom)	
	(false-atom)
	(none-atom)
	(number-atom)
	(list-atom
		(lis List?)))
	
(define-datatype List List?
	(a-list
		(lis (non-empty-list-of Expressions?))))
	
(define-datatype Expressions Expressions?
	(single-expression
		(expression Expression?))
	(multiple-expressions
		(expressions Expressions?)
		(expression Expression?)))
		

