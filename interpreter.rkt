#lang racket

(require (lib "eopl.ss"  "eopl"))
(require "grammar-datatypes.rkt")

(provide interpret-Program)



; environtment

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val Expresed-value?)
   (env environment?))
  (extend-env-rec
   (p-name symbol?)
   (b-var (list-of symbol?))
   (body expression?)
   (env environment?)))


; value holders


(define-datatype Exprssed-Value Expresed-Value?
	(number-container
		(num Number?))
	(function-container
		(function Function-Definition?)))


(define-datatype Number Number?
	(int-number
		(int number?))
	(float-number
		(float number?))
	(bool-number
		(bool boolean?)))


; interpret grammar datatypes


(define interpret-Program
	(lambda (program-var)
		(cases Program program-var 
			(program (statements)
				(begin 
					(interpret-Statements statements (empty-env))
				))

			(else (displayln "ooops")))))	


(define interpret-Statements
	(lambda (statements-var env)
		(cases Statements statements-var 
			(single-statement (statement)
				(begin 
					(interpret-Statement statement env)	
				))

			(multiple-statements (statements statement)
				(begin 
					(interpret-Statements statements env)
					(interpret-Statement statement env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Statement
	(lambda (statement-var env)
		(cases Statement statement-var 
			(compound-statement-statement (compound-statement)
				(begin 
					(interpret-Compound-Statement compound-statement env)	
				))

			(simple-statement-statement (simple-statement)
				(begin 
					(interpret-Simple-Statement simple-statement env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Simple-Statement
	(lambda (simple-statement-var env)
		(cases Simple-Statement simple-statement-var 
			(assignment-simple-statement (assignment)
				(begin 
					(interpret-Assignment assignment env)	
				))

			(return-simple-statement (return-statement)
				(begin 
					(interpret-Return-Statement return-statement env)	
				))

			(pass-simple-statement ()
				(begin 
					(void) 	
				))

			(else (displayln "ooops")))))	


(define interpret-Compound-Statement
	(lambda (compound-statement-var env)
		(cases Compound-Statement compound-statement-var 
			(function-definition-compound-statement (function-definition)
				(begin 
					(interpret-Function-Definition function-definition env)	
				))

			(if-compound-statment (if-statement)
				(begin 
					(interpret-If-Statement if-statement env)	
				))

			(for-statement-compound-statement (for-statement)
				(begin 
					(interpret-For-Statement for-statement env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Assignment
	(lambda (assignment-var env)
		(cases Assignment assignment-var 
			(assignment (id expression)
				(begin 
					(interpret-Expression expression env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Return-Statement
	(lambda (return-statement-var env)
		(cases Return-Statement return-statement-var 
			(simple-return ()
				(begin 
					(void) 	
				))

			(expression-return (expression)
				(begin 
					(interpret-Expression expression env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Function-Definition
	(lambda (function-definition-var env)
		(cases Function-Definition function-definition-var 
			(function-with-params (id params statements)
				(begin 
					(interpret-Params params env)
					(interpret-Statements statements env)	
				))

			(function-with-no-param (id statements)
				(begin 
					(interpret-Statements statements env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Params
	(lambda (params-var env)
		(cases Params params-var 
			(single-param (param)
				(begin 
					(interpret-Param param env)	
				))

			(multiple-params (params param)
				(begin 
					(interpret-Params params env)
					(interpret-Param param env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Param
	(lambda (param-var env)
		(cases Param param-var 
			(param (id default-value)
				(begin 
					(interpret-Expression default-value env)	
				))

			(else (displayln "ooops")))))	


(define interpret-If-Statement
	(lambda (if-statement-var env)
		(cases If-Statement if-statement-var 
			(if-statement (if-expression if-body else-body)
				(begin 
					(interpret-Expression if-expression env)
					(interpret-Statements if-body env)
					(interpret-Else-Block else-body env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Else-Block
	(lambda (else-block-var env)
		(cases Else-Block else-block-var 
			(else-block (else-statements)
				(begin 
					(interpret-Statements else-statements env)	
				))

			(else (displayln "ooops")))))	


(define interpret-For-Statement
	(lambda (for-statement-var env)
		(cases For-Statement for-statement-var 
			(for (id expression statements)
				(begin 
					(interpret-Expression expression env)
					(interpret-Statements statements env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Expression
	(lambda (expression-var env)
		(cases Expression expression-var 
			(disjunction-expression (disjunction)
				(begin 
					(interpret-Disjunction disjunction env)	
				))

			(sum-expression (sum)
				(begin 
					(interpret-Sum sum env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Disjunction
	(lambda (disjunction-var env)
		(cases Disjunction disjunction-var 
			(conjunction-disjunction (conjunction)
				(begin 
					(interpret-Conjunction conjunction env)	
				))

			(or-disjunction (disjunction conjunction)
				(begin 
					(interpret-Disjunction disjunction env)
					(interpret-Conjunction conjunction env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Conjunction
	(lambda (conjunction-var env)
		(cases Conjunction conjunction-var 
			(inversion-conjunction (inversion)
				(begin 
					(interpret-Inversion inversion env)	
				))

			(and-conjunction (conjunction inversion)
				(begin 
					(interpret-Conjunction conjunction env)
					(interpret-Inversion inversion env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Inversion
	(lambda (inversion-var env)
		(cases Inversion inversion-var 
			(not-inversion (inversion)
				(begin 
					(interpret-Inversion inversion env)	
				))

			(comparison-inversion (comparison)
				(begin 
					(interpret-Comparison comparison env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Comparison
	(lambda (comparison-var env)
		(cases Comparison comparison-var 
			(eq-sum-comparison (eq-sum)
				(begin 
					(interpret-Eq-Sum eq-sum env)	
				))

			(lt-sum-comparison (lt-sum)
				(begin 
					(interpret-Lt-Sum lt-sum env)	
				))

			(gt-sum-comparison (gt-sum)
				(begin 
					(interpret-Gt-Sum gt-sum env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Eq-Sum
	(lambda (eq-sum-var env)
		(cases Eq-Sum eq-sum-var 
			(eq-sum (left-sum right-sum)
				(begin 
					(interpret-Sum left-sum env)
					(interpret-Sum right-sum env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Lt-Sum
	(lambda (lt-sum-var env)
		(cases Lt-Sum lt-sum-var 
			(lt-sum (left-sum right-sum)
				(begin 
					(interpret-Sum left-sum env)
					(interpret-Sum right-sum env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Gt-Sum
	(lambda (gt-sum-var env)
		(cases Gt-Sum gt-sum-var 
			(gt-sum (left-sum right-sum)
				(begin 
					(interpret-Sum left-sum env)
					(interpret-Sum right-sum env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Sum
	(lambda (sum-var env)
		(cases Sum sum-var 
			(addition (sum term)
				(begin 
					(interpret-Sum sum env)
					(interpret-Term term env)	
				))

			(subtraction (sum term)
				(begin 
					(interpret-Sum sum env)
					(interpret-Term term env)	
				))

			(term-sum (term)
				(begin 
					(interpret-Term term env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Term
	(lambda (term-var env)
		(cases Term term-var 
			(muliplication (term factor)
				(begin 
					(interpret-Term term env)
					(interpret-Factor factor env)	
				))

			(division (term factor)
				(begin 
					(interpret-Term term env)
					(interpret-Factor factor env)	
				))

			(factor-term (factor)
				(begin 
					(interpret-Factor factor env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Factor
	(lambda (factor-var env)
		(cases Factor factor-var 
			(plus (factor)
				(begin 
					(interpret-Factor factor env)	
				))

			(minus (factor)
				(begin 
					(interpret-Factor factor env)	
				))

			(power-factor (power)
				(begin 
					(interpret-Power power env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Power
	(lambda (power-var env)
		(cases Power power-var 
			(power (atom factor)
				(begin 
					(interpret-Atom atom env)
					(interpret-Factor factor env)	
				))

			(primary-power (primary)
				(begin 
					(interpret-Primary primary env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Primary
	(lambda (primary-var env)
		(cases Primary primary-var 
			(atom-primary (atom)
				(begin 
					(interpret-Atom atom env)	
				))

			(array-ref (primary expression)
				(begin 
					(interpret-Primary primary env)
					(interpret-Expression expression env)	
				))

			(simple-call (primary)
				(begin 
					(interpret-Primary primary env)	
				))

			(argument-call (primary arguments)
				(begin 
					(interpret-Primary primary env)
					(interpret-Arguments arguments env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Arguments
	(lambda (arguments-var env)
		(cases Arguments arguments-var 
			(single-argument (expression)
				(begin 
					(interpret-Expression expression env)	
				))

			(multiple-arguments (arguments expression)
				(begin 
					(interpret-Arguments arguments env)
					(interpret-Expression expression env)	
				))

			(else (displayln "ooops")))))	


(define interpret-Atom
	(lambda (atom-var env)
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
					(interpret-List lis env)	
				))

			(else (displayln "ooops")))))	


(define interpret-List
	(lambda (list-var env)
		(cases List list-var 
			(expression-list (expressions)
				(begin 
					(interpret-Expressions expressions env)	
				))

			(empty-list ()
				(begin 
					(void) 	
				))

			(else (displayln "ooops")))))	


(define interpret-Expressions
	(lambda (expressions-var env)
		(cases Expressions expressions-var 
			(single-expression (expression)
				(begin 
					(interpret-Expression expression env)	
				))

			(multiple-expressions (expressions expression)
				(begin 
					(interpret-Expressions expressions env)
					(interpret-Expression expression env)	
				))

			(else (displayln "ooops")))))	

