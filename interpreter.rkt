#lang racket

(require (lib "eopl.ss"  "eopl"))


	(define interpret-Program
		(lambda (program-var)
			(cases Program program-var
				
				(program (statements)
					(begin
						 (interpret-Statements statements)
	
					))


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


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
							
					))


				(else )
									 )))	


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


				(else )
									 )))	


	(define interpret-Assignment
		(lambda (assignment-var)
			(cases Assignment assignment-var
				
				(assignment (id expression)
					(begin
						 (interpret-symbol id)
 (interpret-Expression expression)
	
					))


				(else )
									 )))	


	(define interpret-Return-Statement
		(lambda (return-statement-var)
			(cases Return-Statement return-statement-var
				
				(simple-return ()
					(begin
							
					))


				(expression-return (expression)
					(begin
						 (interpret-Expression expression)
	
					))


				(else )
									 )))	


	(define interpret-Function-Definition
		(lambda (function-definition-var)
			(cases Function-Definition function-definition-var
				
				(function-with-params (id params statements)
					(begin
						 (interpret-symbol id)
 (interpret-Params params)
 (interpret-Statements statements)
	
					))


				(function-with-no-param (id statements)
					(begin
						 (interpret-symbol id)
 (interpret-Statements statements)
	
					))


				(else )
									 )))	


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


				(else )
									 )))	


	(define interpret-Param
		(lambda (param-var)
			(cases Param param-var
				
				(param (id default-value)
					(begin
						 (interpret-symbol id)
 (interpret-Expression default-value)
	
					))


				(else )
									 )))	


	(define interpret-If-Statement
		(lambda (if-statement-var)
			(cases If-Statement if-statement-var
				
				(if-statement (if-expression if-body else-body)
					(begin
						 (interpret-Expression if-expression)
 (interpret-Statements if-body)
 (interpret-Else-Block else-body)
	
					))


				(else )
									 )))	


	(define interpret-Else-Block
		(lambda (else-block-var)
			(cases Else-Block else-block-var
				
				(else-block (else-statements)
					(begin
						 (interpret-Statements else-statements)
	
					))


				(else )
									 )))	


	(define interpret-For-Statement
		(lambda (for-statement-var)
			(cases For-Statement for-statement-var
				
				(for (id expression statements)
					(begin
						 (interpret-symbol id)
 (interpret-Expression expression)
 (interpret-Statements statements)
	
					))


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


	(define interpret-Eq-Sum
		(lambda (eq-sum-var)
			(cases Eq-Sum eq-sum-var
				
				(eq-sum (left-sum right-sum)
					(begin
						 (interpret-Sum left-sum)
 (interpret-Sum right-sum)
	
					))


				(else )
									 )))	


	(define interpret-Lt-Sum
		(lambda (lt-sum-var)
			(cases Lt-Sum lt-sum-var
				
				(lt-sum (left-sum right-sum)
					(begin
						 (interpret-Sum left-sum)
 (interpret-Sum right-sum)
	
					))


				(else )
									 )))	


	(define interpret-Gt-Sum
		(lambda (gt-sum-var)
			(cases Gt-Sum gt-sum-var
				
				(gt-sum (left-sum right-sum)
					(begin
						 (interpret-Sum left-sum)
 (interpret-Sum right-sum)
	
					))


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


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


				(else )
									 )))	


	(define interpret-primary
		(lambda (primary-var)
			(cases primary primary-var
				
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


				(else )
									 )))	


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


				(else )
									 )))	


	(define interpret-Atom
		(lambda (atom-var)
			(cases Atom atom-var
				
				(id-atom (id)
					(begin
						 (interpret-symbol id)
	
					))


				(true-atom ()
					(begin
							
					))


				(false-atom ()
					(begin
							
					))


				(none-atom ()
					(begin
							
					))


				(number-atom (number)
					(begin
						 (interpret-number number)
	
					))


				(list-atom (lis)
					(begin
						 (interpret-List lis)
	
					))


				(else )
									 )))	


	(define interpret-List
		(lambda (list-var)
			(cases List list-var
				
				(expression-list (expressions)
					(begin
						 (interpret-Expressions expressions)
	
					))


				(empty-list ()
					(begin
							
					))


				(else )
									 )))	


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


				(else )
									 )))	

