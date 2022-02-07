#lang racket

(require (lib "eopl.ss"  "eopl"))
(require racket/promise)
(require "grammar-datatypes.rkt")


(provide interpret-Program)


; environtment

(define-datatype Environment Environment?
  (empty-env)
  (extend-env
   (id symbol?)
   (value Expressed-Value?)
   (env Environment?))
	 
  (extend-env-rec
   (id symbol?)
   (func Expressed-Value?)
   (env Environment?))

  (extend-env-func-answer
   (result Expressed-Value?)
   (env Environment?))
  )

(define apply-env
 (lambda (env search-var)
	(cases Environment env
	 (empty-env ()
		(report-error "apply-env called on empty env"))
	 (extend-env (saved-var saved-val saved-env)
                     (if (promise? saved-var)
                         (if (eqv? (force saved-var) search-var)
                             saved-val
                             (apply-env saved-env search-var))
                         (if (eqv? saved-var search-var)
                             saved-val
                             (apply-env saved-env search-var))))
   (extend-env-rec (id func saved-env)
		(if (eqv? search-var id)
		 func
		 (apply-env saved-env search-var)))
	 (extend-env-func-answer (result saved-env)
	 	(apply-env saved-env search-var))
	 )))

(define get-function-res
 (lambda (env search-var)
	(cases Environment env
	 (empty-env ()
		(none-type '()))
	 (extend-env (saved-var saved-val saved-env)
		 (get-function-res saved-env search-var))
   (extend-env-rec (id func saved-env)
		 (get-function-res saved-env search-var))
	 (extend-env-func-answer (result saved-env)
			result)
	 )))


; value holders


(define-datatype Expressed-Value Expressed-Value?
	(int-number
		(int number?))
	(float-number
		(float number?))
	(boolean
	 (bool boolean?))
	(list-con
	 (lis list?))
	(none-type (non null?))
	(function-expression
		(f Function?))
        (promise
                (p promise?))

  )


(define (Expressed-Value->number expressed-value)
	(cases Expressed-Value expressed-value
		(int-number (int)
			int)
                (promise (p)
                         (Expressed-Value->number (force p)))
	(else (report-error "Expressed-Value->Number called on non number"))
			))


(define (Expressed-Value->bool expressed-value)
	(cases Expressed-Value expressed-value
		(boolean (bool)
			bool)
                (promise (p)
                         (Expressed-Value->bool (force p)))
	(else (report-error "Expressed-Value->bool called on non bool"))
			))

(define (Expressed-Value->function expressed-value)
	(cases Expressed-Value expressed-value
		(function-expression (f)
			f)
                (promise (p)
                         (Expressed-Value->function (force p)))
	(else (report-error "Expressed-Value->function called on non function"))
			))

(define (Expressed-Value->lst expressed-value)
	(cases Expressed-Value expressed-value
	       (list-con (ls) ls)
               (promise (p) (Expressed-Value->lst (force p)))
	       (else (report-error "Expressed-Value->lst called on non list type"))
 		))

(define report-error
	(lambda (err)
		(eopl:error "Error: ~s" err)))


(define-datatype Function Function?
	(func-with-params
		(id symbol?)
		(params Params?)
		(body Statements?)
		(env Environment?))
	(func-with-no-params
		(id symbol?)
		(body Statements?)
		(env Environment?)))


; interpret grammar datatypes

(define func-env (empty-env))

(define interpret-Program-
	(lambda (program-var)
		(cases Program program-var 
			(program (statements)
				(begin 
					(interpret-Statements- statements (empty-env))
				))

			(else (displayln "ooops")))))	


(define interpret-Statements-
	(lambda (statements-var env)
		(cases Statements statements-var 
			(single-statement (statement)
				(begin 
					(interpret-Statement- statement env)	
				))

			(multiple-statements (statements statement)
				(begin 
					(define nenv (interpret-Statements- statements env)) 
					(interpret-Statement- statement nenv))	
				)

			(else (displayln "ooops")))))	


(define interpret-Statement-
	(lambda (statement-var env)
		(cases Statement statement-var 
			(compound-statement-statement (compound-statement)
				(begin 
					(interpret-Compound-Statement- compound-statement env)	
				))

			(else env))))	


(define interpret-Compound-Statement-
	(lambda (compound-statement-var env)
		(cases Compound-Statement compound-statement-var 
			(function-definition-compound-statement (function-definition)
				(begin 
					(interpret-Function-Definition- function-definition env)	
				))

			(else env))))	

(define interpret-Function-Definition-
	(lambda (function-definition-var env)
		(cases Function-Definition function-definition-var 
			(function-with-params (id params statements)
				(begin 
					(set! func-env (extend-env-rec id (function-expression (func-with-params id params statements (empty-env))) func-env))
				))

			(function-with-no-param (id statements)
				(begin 
					(set! func-env (extend-env-rec id (function-expression (func-with-no-params id statements (empty-env))) func-env))
				))

			(else (displayln "ooops")))))	


(define interpret-Program
	(lambda (program-var)
	  	(begin (interpret-Program- program-var)
		(cases Program program-var 
			(program (statements)
				(begin 
					(interpret-Statements statements func-env)
					(displayln "finished")
				))

			(else (displayln "ooops"))))))	


(define interpret-Statements
	(lambda (statements-var env)
		(cases Statements statements-var 
			(single-statement (statement)
				(begin 
					(interpret-Statement statement env)	
				))

			(multiple-statements (statements statement)
				(begin 
					(define nenv (interpret-Statements statements env))
					(if (cadr nenv) nenv 
					  (interpret-Statement statement (car nenv)))	
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
			(print-simple-statement (print-stmt)
				(interpret-Print-Statement print-stmt env)
			 )

			(pass-simple-statement ()
				(begin 
					(list env #f) 	
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
					(define value (delayed-Expression expression env))
					(list (extend-env id value env) #f)
				))

			(else (displayln "ooops")))))	

(define interpret-Print-Statement
	(lambda (stmt env)
	  	(cases Print-Statement stmt
		      (print-stmt (args)
 			(begin 
		  	(print-handler (interpret-Arguments args env))
		 	(list env #f))))))

(define Expressed-Value->printable 
(lambda (inp)
	      (cases Expressed-Value inp
                (promise (p) (Expressed-Value->printable (force p)))
		(int-number (num) num)
		(float-number (num) num)
		(boolean (bool) bool)
		(none-type (non) "None")
		(list-con (lis) (begin
				 (define fr (Expressed-Value->printable (car lis)))
				 (if (null? (cdr lis)) (list fr)
				   (cons fr (Expressed-Value->printable (list-con (cdr lis))))
				   )
				 ))
		    (else (displayln "Error"))
		     
		     )
  ))

(define print-handler
  (lambda (ls)
  (begin
    (displayln (Expressed-Value->printable (car ls)))
   (if (null? (cdr ls)) '() (print-handler (cdr ls)))
   )
  ))


(define interpret-Return-Statement
	(lambda (return-statement-var env)
		(cases Return-Statement return-statement-var 
			(simple-return ()
				(begin 
					(list env #t) 	
				))

			(expression-return (expression)
				(begin 
					(define value (interpret-Expression expression env))
					(list (extend-env-func-answer value env) #t)
				))

			(else (displayln "ooops")))))	


(define interpret-Function-Definition
	(lambda (function-definition-var env)
		(cases Function-Definition function-definition-var 
			(function-with-params (id params statements)
				(begin 
					(list env #f)
				))

			(function-with-no-param (id statements)
				(begin 
					(list env #f)
				))

			(else (displayln "ooops")))))	

(define param-number 
  (lambda (params-var)
    (cases Params params-var
	   (single-param (param) 1)
	   (multiple-params (params param) (+ 1 (param-number params)))
	   (else (displayln "oops"))
  )))

(define interpret-Params
	(lambda (params-var given-vals env)
	  (begin (define par-num (param-number params-var))
		(cases Params params-var 
			(single-param (param)
				(begin 
					(interpret-Param param given-vals env)
				))

			(multiple-params (params param)
				(if (> par-num (length given-vals)) 
				(interpret-Param param '() 
					(interpret-Params params given-vals env))
				(interpret-Param param (list (first (reverse given-vals))) 
					(interpret-Params params (reverse (rest (reverse given-vals))) env)))
				)
			(else (report-error "!!!"))
			))))	


(define interpret-Param
	(lambda (param-var given-val env)
		(cases Param param-var 
			(param (id default-value)
				(begin
					(define val (if (null? given-val) (interpret-Expression default-value env) 
						     (car given-val)))
					(extend-env id val env)
				))

			(else (displayln "ooops")))))	


(define interpret-If-Statement
	(lambda (if-statement-var env)
		(cases If-Statement if-statement-var 
			(if-statement (if-expression if-body else-body)
				(begin 
					(define val (interpret-Expression if-expression env))
					(define value (Expressed-Value->bool val))
					(if value
						(interpret-Statements if-body env)
						(interpret-Else-Block else-body env))
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


(define for-handler
	(lambda (id ls statements env)
	  (begin
	     (define nenv (interpret-Statements statements (extend-env id (car ls) env)))
	     (if (or (cadr nenv) (null? (cdr ls))) nenv
	       (for-handler id (cdr ls) statements (car nenv)) 
	       )
	    )
	  ))

(define interpret-For-Statement
	(lambda (for-statement-var env)
		(cases For-Statement for-statement-var 
			(for (id expression statements)
				(begin 
					(define ls (Expressed-Value->lst (interpret-Expression expression env)))
					( if ls
					(for-handler id ls statements env)
					(list env #f)
					)
				))

			(else (displayln "ooops")))))	

(define delayed-Expression
  (lambda (expression-var env)
		(cases Expression expression-var 
			(disjunction-expression (disjunction)
				(begin 
					(promise (delay (interpret-Disjunction disjunction env)))
				))

			(sum-expression (sum)
				(begin 
					(promise (delay (interpret-Sum sum env)))
				))

			(else (displayln "ooops"))))
  )

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
					(define left (interpret-Disjunction disjunction env))
					(define right (interpret-Conjunction conjunction env))
					(define left-bool (Expressed-Value->bool left))
					(define right-bool (Expressed-Value->bool right))
					(boolean (or left-bool right-bool))
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
					(define left (interpret-Conjunction conjunction env))
					(define right (interpret-Inversion inversion env))
					(define left-bool (Expressed-Value->bool left))
					(define right-bool (Expressed-Value->bool right))
					(boolean (and left-bool right-bool))
				))

			(else (displayln "ooops")))))	


(define interpret-Inversion
	(lambda (inversion-var env)
		(cases Inversion inversion-var 
			(not-inversion (inversion)
				(begin 
					(define value (interpret-Inversion inversion env))
					(define bool-val (Expressed-Value->bool value))
					(boolean (not bool-val))
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
					(define left (interpret-Sum left-sum env))
					(define right (interpret-Sum right-sum env))
					(define left-num (Expressed-Value->number left))
					(define right-num (Expressed-Value->number right))
					(boolean (equal? left-num right-num))
				))

			(else (displayln "ooops")))))	


(define interpret-Lt-Sum
	(lambda (lt-sum-var env)
		(cases Lt-Sum lt-sum-var 
			(lt-sum (left-sum right-sum)
				(begin 
					(define left (interpret-Sum left-sum env))
					(define right (interpret-Sum right-sum env))
					(define left-num (Expressed-Value->number left))
					(define right-num (Expressed-Value->number right))
					(boolean (< left-num right-num))
				))

			(else (displayln "ooops")))))	


(define interpret-Gt-Sum
	(lambda (gt-sum-var env)
		(cases Gt-Sum gt-sum-var 
			(gt-sum (left-sum right-sum)
				(begin 
					(define left (interpret-Sum left-sum env))
					(define right (interpret-Sum right-sum env))
					(define left-num (Expressed-Value->number left))
					(define right-num (Expressed-Value->number right))
					(boolean (> left-num right-num))
				))

			(else (displayln "ooops")))))	


(define interpret-Sum
	(lambda (sum-var env)
		(cases Sum sum-var 
			(addition (sum term)
				(begin 
					(define left (interpret-Sum sum env))
					(define right (interpret-Term term env))
					(displayln left)
					(displayln right)
					(cases Expressed-Value left
						(int-number (lf)
							(define right-num (Expressed-Value->number right))
							(int-number (+ lf right-num))
							)
						(float-number (lf)
							(define right-num (Expressed-Value->number right))
						 	(float-number (+ lf right-num)))
						(list-con (lf)
							(define right-ls (Expressed-Value->lst right))
							(list-con (append lf right-ls)))
						(else (displayln "oops"))
					)
				))

			(subtraction (sum term)
				(begin 
				  (define left (interpret-Sum sum env))
				  (define right (interpret-Term term env))
					(define left-num (Expressed-Value->number left))
					(define right-num (Expressed-Value->number right))
					(int-number (- left-num right-num))
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
					(define left (interpret-Term term env))
                                        (define left-num (Expressed-Value->number left))

                                        (if (eqv? left-num 0)
                                            (int-number left-num)
                                            (let* ([right (interpret-Factor factor env)] [right-num (Expressed-Value->number right)])
                                              (int-number (* left-num right-num))))

                                        
				))

			(division (term factor)
				(begin 
					(define left (interpret-Term term env))
					(define right (interpret-Factor factor env))
					(define left-num (Expressed-Value->number left))
					(define right-num (Expressed-Value->number right))
					(int-number (/ left-num right-num))
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
					(define value (interpret-Factor factor env))
					(int-number (* -1 (Expressed-Value->number value)))
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
					(define base (interpret-Atom atom env))
					(define pow (interpret-Factor factor env))
					(define base-num (Expressed-Value->number base))
					(define pow-num (Expressed-Value->number pow))
					(int-number (expt base-num pow-num))
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

			(array-ref (primary expression) (begin 
					(define arr (Expressed-Value->lst (interpret-Primary primary env)))
					(list-ref arr (Expressed-Value->number (interpret-Expression expression env)))	
				))

			(simple-call (primary)
				(begin 
					(define func (Expressed-Value->function (interpret-Primary primary env)))
					(define res (call-function func empty))
					res
				))

			(argument-call (primary arguments)
				(begin 
					(define func (Expressed-Value->function (interpret-Primary primary env)))
					
					; handle print here of func is print!
					
					(define argument-values (interpret-Arguments arguments env))
					(call-function func argument-values)
				))

			(else (displayln "ooops")))))	


(define call-function
	(lambda (func argument-values)
		(cases Function func
			(func-with-params (id params body env)
				(begin
					(define nenv (interpret-Params params argument-values func-env))
					(define ret-env (interpret-Statements body nenv))
					(get-function-res (car ret-env) id)
				))
			 (func-with-no-params (id body env)
			 	(begin
					(define ret-env (interpret-Statements body func-env))
					(define sss (get-function-res (car ret-env) id))
					sss
				))
			(else (displayln "oops"))
			)))
				
				

(define interpret-Arguments
	(lambda (arguments-var env)
		(cases Arguments arguments-var 
			(single-argument (expression)
				(begin 
					(list (interpret-Expression expression env))
				))

			(multiple-arguments (arguments expression)
				(begin 
					(append (interpret-Arguments arguments env)
						(list (interpret-Expression expression env)))
				))

			(else (displayln "ooops")))))	


(define interpret-Atom
	(lambda (atom-var env)
		(cases Atom atom-var 
			(id-atom (id)
				(begin 
					(apply-env env id)
				))

			(true-atom ()
				(begin 
					(boolean #t)
				))

			(false-atom ()
				(begin 
					(boolean #f)
				))

			(none-atom ()
				(begin 
					(void) 	
				))

			(number-atom (number)
				(begin 
					; todo: maybe create float here bases on number's representation
					(int-number number)
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
					(list-con (interpret-Expressions expressions env))	
				))

			(empty-list ()
				(begin 
					(list-con '()) 	
				))

			(else (displayln "ooops")))))	


(define interpret-Expressions
	(lambda (expressions-var env)
		(cases Expressions expressions-var 
			(single-expression (expression)
				(begin 
					(cons (interpret-Expression expression env) '())
				))

			(multiple-expressions (expressions expression)
				(begin 
					(append (interpret-Expressions expressions env)
					(cons (interpret-Expression expression env) '()))	
				))

			(else (displayln "ooops")))))	

