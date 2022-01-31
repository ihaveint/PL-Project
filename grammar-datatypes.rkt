#lang racket

(provide program single-statement multiple-statements
         compound-statement-statement simple-statement-statement
         assignment-simple-statement return-simple-statement
         pass-simple-statement function-definition-compound-statement
         if-compound-statment for-statement-compound-statement assignment
         simple-return expression-return function-with-params
         function-with-no-param if-statement
         else-block for disjunction-expression sum-expression
         conjunction-disjunction or-disjunction inversion-conjunction
         and-conjunction not-inversion comparison-inversion
         eq-sum-comparison lt-sum-comparison gt-sum-comparison
         eq-sum single-param multiple-params param
         lt-sum gt-sum addition subtraction term-sum muliplication
         division factor-term plus minus power-factor power
         primary-power atom-primary array-ref simple-call argument-call
         single-argument multiple-arguments id-atom true-atom false-atom
         none-atom number-atom list-atom expression-list single-expression
         multiple-expressions empty-list)

(require (lib "eopl.ss"  "eopl"))


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
		(if-body Statements?)
		(else-body Else-Block?)))

(define-datatype Else-Block Else-Block?
	(else-block
		(else-statements Statements?)))

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
		(eq-sum Eq-Sum?))
	(lt-sum-comparison
		(lt-sum Lt-Sum?))
	(gt-sum-comparison
		(gt-sum Gt-Sum?)))


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
		(term Term?))
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
	(number-atom
                (number number?))
	(list-atom
		(lis List?)))
	
(define-datatype List List?
	(expression-list
		(expressions Expressions?))
	(empty-list))
	
(define-datatype Expressions Expressions?
	(single-expression
		(expression Expression?))
	(multiple-expressions
		(expressions Expressions?)
		(expression Expression?)))
		

