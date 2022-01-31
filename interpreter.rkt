#lang racket

(require (lib "eopl.ss"  "eopl"))
(require "grammar-datatypes.rkt")


(define interpret-Program
	(lambda (program-var)
		(cases Program program-var 
			(program (statements)
				(begin 
					(interpret-Statements statements)	
				))

			(else (displayln "ooops")))))	


(define interpret-Statements
	(lambda (statements-var)
		(cases Statements statements-var 
			(single-statement (statement)
				(begin 
					(interpret-Statement statement)	
				))

			(multiple-statements (statements statement)
				(begin 
					(interpret-Statements statements)
					(interpret-Statement statement)	
				))

			(else (displayln "ooops")))))	


(define interpret-Statement
	(lambda (statement-var)
		(cases Statement statement-var 
			(compound-statement-statement (compound-statement)
				(begin 
					(interpret-Compound-Statement compound-statement)	
				))

			(simple-statement-statement (simple-statement)
				(begin 
					(interpret-Simple-Statement simple-statement)	
				))

			(else (displayln "ooops")))))	


(define interpret-Simple-Statement
	(lambda (simple-statement-var)
		(cases Simple-Statement simple-statement-var 
			(assignment-simple-statement (assignment)
				(begin 
					(interpret-Assignment assignment)	
				))

			(return-simple-statement (return-statement)
				(begin 
					(interpret-Return-Statement return-statement)	
				))

			(pass-simple-statement ()
				(begin 
					(void) 	
				))

			(else (displayln "ooops")))))	


(define interpret-Compound-Statement
	(lambda (compound-statement-var)
		(cases Compound-Statement compound-statement-var 
			(function-definition-compound-statement (function-definition)
				(begin 
					(interpret-Function-Definition function-definition)	
				))

			(if-compound-statment (if-statement)
				(begin 
					(interpret-If-Statement if-statement)	
				))

			(for-statement-compound-statement (for-statement)
				(begin 
					(interpret-For-Statement for-statement)	
				))

			(else (displayln "ooops")))))	


(define interpret-Assignment
	(lambda (assignment-var)
		(cases Assignment assignment-var 
			(assignment (id expression)
				(begin 
					(interpret-Expression expression)	
				))

			(else (displayln "ooops")))))	


(define interpret-Return-Statement
	(lambda (return-statement-var)
		(cases Return-Statement return-statement-var 
			(simple-return ()
				(begin 
					(void) 	
				))

			(expression-return (expression)
				(begin 
					(interpret-Expression expression)	
				))

			(else (displayln "ooops")))))	


(define interpret-Function-Definition
	(lambda (function-definition-var)
		(cases Function-Definition function-definition-var 
			(function-with-params (id params statements)
				(begin 
					(interpret-Params params)
					(interpret-Statements statements)	
				))

			(function-with-no-param (id statements)
				(begin 
					(interpret-Statements statements)	
				))

			(else (displayln "ooops")))))	


(define interpret-Params
	(lambda (params-var)
		(cases Params params-var 
			(single-param (param)
				(begin 
					(interpret-Param param)	
				))

			(multiple-params (params param)
				(begin 
					(interpret-Params params)
					(interpret-Param param)	
				))

			(else (displayln "ooops")))))	


(define interpret-Param
	(lambda (param-var)
		(cases Param param-var 
			(param (id default-value)
				(begin 
					(interpret-Expression default-value)	
				))

			(else (displayln "ooops")))))	


(define interpret-If-Statement
	(lambda (if-statement-var)
		(cases If-Statement if-statement-var 
			(if-statement (if-expression if-body else-body)
				(begin 
					(interpret-Expression if-expression)
					(interpret-Statements if-body)
					(interpret-Else-Block else-body)	
				))

			(else (displayln "ooops")))))	


(define interpret-Else-Block
	(lambda (else-block-var)
		(cases Else-Block else-block-var 
			(else-block (else-statements)
				(begin 
					(interpret-Statements else-statements)	
				))

			(else (displayln "ooops")))))	


(define interpret-For-Statement
	(lambda (for-statement-var)
		(cases For-Statement for-statement-var 
			(for (id expression statements)
				(begin 
					(interpret-Expression expression)
					(interpret-Statements statements)	
				))

			(else (displayln "ooops")))))	


(define interpret-Expression
	(lambda (expression-var)
		(cases Expression expression-var 
			(disjunction-expression (disjunction)
				(begin 
					(interpret-Disjunction disjunction)	
				))

			(sum-expression (sum)
				(begin 
					(interpret-Sum sum)	
				))

			(else (displayln "ooops")))))	


(define interpret-Disjunction
	(lambda (disjunction-var)
		(cases Disjunction disjunction-var 
			(conjunction-disjunction (conjunction)
				(begin 
					(interpret-Conjunction conjunction)	
				))

			(or-disjunction (disjunction conjunction)
				(begin 
					(interpret-Disjunction disjunction)
					(interpret-Conjunction conjunction)	
				))

			(else (displayln "ooops")))))	


(define interpret-Conjunction
	(lambda (conjunction-var)
		(cases Conjunction conjunction-var 
			(inversion-conjunction (inversion)
				(begin 
					(interpret-Inversion inversion)	
				))

			(and-conjunction (conjunction inversion)
				(begin 
					(interpret-Conjunction conjunction)
					(interpret-Inversion inversion)	
				))

			(else (displayln "ooops")))))	


(define interpret-Inversion
	(lambda (inversion-var)
		(cases Inversion inversion-var 
			(not-inversion (inversion)
				(begin 
					(interpret-Inversion inversion)	
				))

			(comparison-inversion (comparison)
				(begin 
					(interpret-Comparison comparison)	
				))

			(else (displayln "ooops")))))	


(define interpret-Comparison
	(lambda (comparison-var)
		(cases Comparison comparison-var 
			(eq-sum-comparison (eq-sum)
				(begin 
					(interpret-Eq-Sum eq-sum)	
				))

			(lt-sum-comparison (lt-sum)
				(begin 
					(interpret-Lt-Sum lt-sum)	
				))

			(gt-sum-comparison (gt-sum)
				(begin 
					(interpret-Gt-Sum gt-sum)	
				))

			(else (displayln "ooops")))))	


(define interpret-Eq-Sum
	(lambda (eq-sum-var)
		(cases Eq-Sum eq-sum-var 
			(eq-sum (left-sum right-sum)
				(begin 
					(interpret-Sum left-sum)
					(interpret-Sum right-sum)	
				))

			(else (displayln "ooops")))))	


(define interpret-Lt-Sum
	(lambda (lt-sum-var)
		(cases Lt-Sum lt-sum-var 
			(lt-sum (left-sum right-sum)
				(begin 
					(interpret-Sum left-sum)
					(interpret-Sum right-sum)	
				))

			(else (displayln "ooops")))))	


(define interpret-Gt-Sum
	(lambda (gt-sum-var)
		(cases Gt-Sum gt-sum-var 
			(gt-sum (left-sum right-sum)
				(begin 
					(interpret-Sum left-sum)
					(interpret-Sum right-sum)	
				))

			(else (displayln "ooops")))))	


(define interpret-Sum
	(lambda (sum-var)
		(cases Sum sum-var 
			(addition (sum term)
				(begin 
					(interpret-Sum sum)
					(interpret-Term term)	
				))

			(subtraction (sum term)
				(begin 
					(interpret-Sum sum)
					(interpret-Term term)	
				))

			(term-sum (term)
				(begin 
					(interpret-Term term)	
				))

			(else (displayln "ooops")))))	


(define interpret-Term
	(lambda (term-var)
		(cases Term term-var 
			(muliplication (term factor)
				(begin 
					(interpret-Term term)
					(interpret-Factor factor)	
				))

			(division (term factor)
				(begin 
					(interpret-Term term)
					(interpret-Factor factor)	
				))

			(factor-term (factor)
				(begin 
					(interpret-Factor factor)	
				))

			(else (displayln "ooops")))))	


(define interpret-Factor
	(lambda (factor-var)
		(cases Factor factor-var 
			(plus (factor)
				(begin 
					(interpret-Factor factor)	
				))

			(minus (factor)
				(begin 
					(interpret-Factor factor)	
				))

			(power-factor (power)
				(begin 
					(interpret-Power power)	
				))

			(else (displayln "ooops")))))	


(define interpret-Power
	(lambda (power-var)
		(cases Power power-var 
			(power (atom factor)
				(begin 
					(interpret-Atom atom)
					(interpret-Factor factor)	
				))

			(primary-power (primary)
				(begin 
					(interpret-Primary primary)	
				))

			(else (displayln "ooops")))))	


(define interpret-Primary
	(lambda (primary-var)
		(cases Primary primary-var 
			(atom-primary (atom)
				(begin 
					(interpret-Atom atom)	
				))

			(array-ref (primary expression)
				(begin 
					(interpret-Primary primary)
					(interpret-Expression expression)	
				))

			(simple-call (primary)
				(begin 
					(interpret-Primary primary)	
				))

			(argument-call (primary arguments)
				(begin 
					(interpret-Primary primary)
					(interpret-Arguments arguments)	
				))

			(else (displayln "ooops")))))	


(define interpret-Arguments
	(lambda (arguments-var)
		(cases Arguments arguments-var 
			(single-argument (expression)
				(begin 
					(interpret-Expression expression)	
				))

			(multiple-arguments (arguments expression)
				(begin 
					(interpret-Arguments arguments)
					(interpret-Expression expression)	
				))

			(else (displayln "ooops")))))	


(define interpret-Atom
	(lambda (atom-var)
		(cases Atom atom-var 
			(id-atom (id)
				(begin 
					(void) 	
				))

			(true-atom ()
				(begin 
					(void) 	
				))

			(false-atom ()
				(begin 
					(void) 	
				))

			(none-atom ()
				(begin 
					(void) 	
				))

			(number-atom (number)
				(begin 
					(void) 	
				))

			(list-atom (lis)
				(begin 
					(interpret-List lis)	
				))

			(else (displayln "ooops")))))	


(define interpret-List
	(lambda (list-var)
		(cases List list-var 
			(expression-list (expressions)
				(begin 
					(interpret-Expressions expressions)	
				))

			(empty-list ()
				(begin 
					(void) 	
				))

			(else (displayln "ooops")))))	


(define interpret-Expressions
	(lambda (expressions-var)
		(cases Expressions expressions-var 
			(single-expression (expression)
				(begin 
					(interpret-Expression expression)	
				))

			(multiple-expressions (expressions expression)
				(begin 
					(interpret-Expressions expressions)
					(interpret-Expression expression)	
				))

			(else (displayln "ooops")))))	

