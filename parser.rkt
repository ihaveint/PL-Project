#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require "grammar-datatypes.rkt")
(require "interpreter.rkt")


(define-tokens value-tokens (NUM ID BOOL))
(define-empty-tokens op-tokens (EOF plus comma lBracket rBracket lParen rParen power minus division mult greater 
                            less equal not and or for in colon else double_equal def EMPTY_PARAMS return pass semicolon NONE if True False))

(define-lex-abbrevs
 (digit (:/ "0" "9"))
 (char-or-digit (:or (:/ "a" "z") (:/ "A" "Z") (:/ "0" "9")))
 (char (:or (:/ "a" "z") (:/ "A" "Z"))))

(define lexer-imp
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space #\newline)
    (lexer-imp input-port)]
   [(:: (:+ digit) (:? ".") (:* digit))
    (token-NUM (string->number lexeme))]
   ["+" (token-plus)]
   ["-" (token-minus)]
   ["*" (token-mult)]
   ["**" (token-power)]
   ["==" (token-double_equal)]
   ["=" (token-equal)]
   ["," (token-comma)]
   ["and" (token-and)]
   ["or" (token-or)]
   [";" (token-semicolon)]
   ["/" (token-division)]
   [":" (token-colon)]
   [">" (token-greater)]
   ["<" (token-less)]
   ["if" (token-if)]
   ["else" (token-else)]
   ["for" (token-for)]
   ["in" (token-in)]
   ["()" (token-EMPTY_PARAMS)]
   ["[" (token-rBracket)]
   [")" (token-rParen)]
   ["[" (token-lBracket)]
   ["(" (token-lParen)]
   ["True" (token-True)]
   ["False" (token-False)]
   ["None" (token-NONE)]
   ["def" (token-def)]
   ["return" (token-return)]
   ["pass" (token-pass)]
   [(:: (:+ char))
    (token-ID (string->symbol lexeme))]
   
   ))


(define simple-math-parser
  (parser
   (start Program)
   (end EOF)
   (error void)
   (tokens op-tokens value-tokens)
   (grammar
    (Program ((Statements) (program $1)))
    (Statements ((Statement semicolon) (single-statement $1))
                ((Statements Statement semicolon) (multiple-statements $1 $2)))
    (Statement ((Compound_stmts) (compound-statement-statement $1))
                ((Simple_stmt) (simple-statement-statement $1)))
    (Simple_stmt ((Assignment) (assignment-simple-statement $1))
                 ((Return_stmt) (return-simple-statement $1))
                 ((pass) (pass-simple-statement)))
    (Compound_stmts ((Function_def) (function-definition-compound-statement $1))
                    ((If_stmt) (if-compound-statment $1))
                    ((For_stmt) (for-statement-compound-statement $1)))
    (Assignment ((ID equal Expression) (assignment $1 $3)))
    (Return_stmt ((return) (simple-return))
                 ((return Expression) (expression-return $2)))
    (Function_def ((def ID lParen Params rParen colon Statements) (function-with-params $2 $4 $7))
                  ((def ID EMPTY_PARAMS colon Statements) (function-with-no-param $2 $5)))
    (Params ((Param_with_default) (single-param $1))
                  ((Params comma Param_with_default) (multiple-params $1 $3)))
    (Param_with_default ((ID equal Expression) (param $1 $3)))
    (If_stmt ((if Expression colon Statements Else_block) (if-statement $2 $4 $5)))
    (Else_block ((else colon Statements) (else-block $3)))
    (For_stmt ((for ID in Expression colon Statements) (for $2 $4 $6)))
    (Expression ((Disjunction) (disjunction-expression $1))
                ((Sum) (sum-expression $1)))
    (Disjunction ((Conjunction) (conjunction-disjunction $1))
                  ((Disjunction or Conjunction) (or-disjunction $1 $3)))
    (Conjunction ((Inversion) (inversion-conjunction $1))
                  ((Conjunction and Inversion) (and-conjunction $1 $3)))
    (Inversion ((not Inversion) (not-inversion $2))
                  ((Comparision) (comparison-inversion $1)))    
    (Comparision ((Eq_Sum) (eq-sum-comparison $1))
                  ((Lt_Sum) (lt-sum-comparison $1))
                  ((Gt_Sum) (gt-sum-comparison $1)))
    (Eq_Sum ((Sum double_equal Sum) (eq-sum $1 $3)))
    (Lt_Sum ((Sum less Sum) (lt-sum $1 $3)))
    (Gt_Sum ((Sum greater Sum) (gt-sum $1 $3)))
    (Sum ((Sum plus Term) (addition $1 $3))
                  ((Sum minus Term) (subtraction $1 $3))
                  ((Term) (term-sum $1)))
    (Term ((Term mult Factor) (muliplication $1 $3))
                  ((Term division Factor) (division $1 $3))
                  ((Factor) (factor-term $1)))
    (Factor ((plus Factor) (plus $2))
                  ((minus Factor) (minus $2))
                  ((Power) (power-factor $1)))
    (Power ((Atom Power Factor) (power $1 $3))
                  ((Primary) (primary-power $1)))
    (Primary ((Atom) (atom-primary $1))
                  ((Primary lBracket Expression rBracket) (array-ref $1 $3))
                  ((Primary lParen rParen) (simple-call $1))
                  ((Primary lParen Arguments rParen) (argument-call $1 $3)))
    (Arguments ((Expression) (single-argument $1))
               ((Arguments comma Expression) (multiple-arguments $1 $3)))
    (Atom ((ID) (id-atom $1))
          ((False) (false-atom))
          ((True) (true-atom))
          ((NONE) (none-atom))
          ((NUM) (number-atom $1))
          ((List) (list-atom $1)))
    (List ((lBracket Expressions rBracket) (expression-list $2))
          ((lBracket rBracket) (empty-list)))
    (Expressions ((Expressions comma Expression) (multiple-expressions $1 $3))
                 ((Expression) (single-expression $1)))
    )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define in (open-input-file "input.py"))
(define my-lexer (lex-this lexer-imp in))
(let ((parser-res (simple-math-parser my-lexer))) (interpret-Program parser-res))
(close-input-port in)
