#lang racket
(require "simpleParser.rkt")

(define operator
  (lambda (expression)
    (car expression)))

(define operant_1
  (lambda (expression)
    (cadr expression)))

(define operant_2
  (lambda (expression)
    (caddr expression)))

(define operant_3
  (lambda (expression)
    (cadddr expression)))

(define have_operant_3
  (lambda (expression)
    (not (null?(cdddr expression)))))

(define pair_value
  (lambda (pair)
    (car (cdr pair))))

(define get_var_value
  (lambda (expression state)
    (cond
      ((null? state) '())
      ((eq? expression (car (car state))) (pair_value (car state)))
      (else (get_var_value expression (cdr state))))))



(define myAnd
  (lambda (op1 op2)
    (if (and (boolean? op1) (boolean? op2))
        (and op1 op2)
        (error 'op "Both operant need to be boolean"))))

(define myOr
  (lambda (op1 op2)
    (if (and (boolean? op1) (boolean? op2))
        (or op1 op2)
        (error 'op "Both operant need to be boolean"))))

(define myNot
  (lambda (op)
    (if (boolean? op)
        (not op)
        (error 'op "Both operant need to be boolean"))))

(define myLarger
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (> op1 op2)
        (error 'op "Both operant need to be integer"))))

(define myLargerEqual
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (>= op1 op2)
        (error 'op "Both operant need to be integer"))))

(define mySmaller
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (< op1 op2)
        (error 'op "Both operant need to be integer"))))

(define mySmallerEqual
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (<= op1 op2)
        (error 'op "Both operant need to be integer"))))

(define myAdd
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (+ op1 op2)
        (error 'op "Both operant need to be integer"))))

(define mySubtract
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (- op1 op2)
        (error 'op "Both operant need to be integer"))))

(define myQuotient
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (quotient op1 op2)
        (error 'op "Both operant need to be integer"))))

(define myMultiply
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (* op1 op2)
        (error 'op "Both operant need to be integer"))))

(define myRemainder
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (modulo op1 op2)
        (error 'op "Both operant need to be integer"))))

(define have_one_operant?
  (lambda (expression)
    (null? (cddr expression))))

(define toKeyValuePair
  (lambda (key value)
    (cons key (cons value '()))))

(define addToState
  (lambda (var value state)
    (cons (toKeyValuePair var value) state)))

(define initialized?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? var (car (car state))) #t)
      (else (initialized? var (cdr state))))))

(define updateState
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((eq? var (car (car state))) (cons (toKeyValuePair var value) (cdr state)))
      (else (cons (car state) (updateState var value (cdr state)))))))



(define myAssign
  (lambda (var value state)
    (if (initialized? var state)
        (updateState var value state)
        (error 'var "variable is not initialized"))))
       
(define myInitialize
  (lambda (var value state)
    (if (initialized? var state)
        (error 'var "variable is initialized")
        (addToState var value state))))

;This traverse through the statements and build up the state 
(define traverseStatements
  (lambda (statements state)
    (if (null? statements)
        state
        (traverseStatements (cdr statements) (M_state (car statements) state)))))

;This method take in a filename, parse it into statements and traverse the state. Then return what is stored in the return variable.
(define runFile
  (lambda (filename)
    (parse_value (get_var_value 'return (traverseStatements (parser filename) '())))))

;Parse resulting value into java-like format. Only accept int or bool
(define parse_value
  (lambda (value)
    (cond
      ((boolean? value) (if value 'true 'false))
      ((integer? value) value)
      (else (error 'value "Value should be int or boolean")))))

(define myIf
  (lambda (expression state)
    (cond
      ((not (boolean? (M_value (operant_1 expression) state))) (error 'expression "condition should be boolean"))
      ((M_value (operant_1 expression) state) (M_state (operant_2 expression) (M_state (operant_1 expression) state)))
      ((not (have_operant_3 expression)) state)
      (else (M_state (operant_3 expression) (M_state (operant_1 expression) state))))))

(define myWhile
  (lambda (expression state)
    (cond
      ((M_value (operant_1 expression) state) (myWhile expression (M_state (operant_2 expression) (M_state (operant_1 expression) state))))
      (else (M_state (operant_1 expression) state)))))

; Apply M_state of operant 1 to M_state of operant 2
(define 2_operants_M_state
  (lambda (expression state)
    (if (have_one_operant? expression)
        (error 'expression "have only one operant")
        (M_state (operant_2 expression) (M_state (operant_1 expression) state)))))
      
; This also consider nested equal sign
(define M_state
  (lambda (expression state)
    (cond
      ((not (list? expression)) state)
      ((eq? (operator expression) 'var) (if (have_one_operant? expression)
                                            (myInitialize (operant_1 expression) '() state)
                                            (myInitialize (operant_1 expression) (M_value (operant_2 expression) state) (M_state (operant_2 expression) state))))
      ((eq? (operator expression) '=) (myAssign (operant_1 expression) (M_value (operant_2 expression) state) (M_state (operant_2 expression) state)))
      ((eq? (operator expression) 'return) (myInitialize 'return (M_value (operant_1 expression) state) (M_state(operant_1 expression) state)))
      ((eq? (operator expression) 'if) (myIf expression state))
      ((eq? (operator expression) 'while) (myWhile expression state))
      ((eq? (operator expression) '&&) (2_operants_M_state expression state))
      ((eq? (operator expression) '||) (2_operants_M_state expression state))
      ((eq? (operator expression) '!) (M_state (operant_1 expression) state))
      ((eq? (operator expression) '>) (2_operants_M_state expression state))
      ((eq? (operator expression) '>=) (2_operants_M_state expression state))
      ((eq? (operator expression) '<) (2_operants_M_state expression state))
      ((eq? (operator expression) '<=) (2_operants_M_state expression state))
      ((eq? (operator expression) '==) (2_operants_M_state expression state))
      ((eq? (operator expression) '!=) (2_operants_M_state expression state))
      ((eq? (operator expression) '+) (2_operants_M_state expression state))
      ((eq? (operator expression) '-) (if (have_one_operant? expression)
                                          (M_state (operant_1 expression) state)
                                          (2_operants_M_state expression state)))
      ((eq? (operator expression) '*) (2_operants_M_state expression state))
      ((eq? (operator expression) '/) (2_operants_M_state expression state))
      ((eq? (operator expression) '%) (2_operants_M_state expression state))
      (else state)
      )))


; This also consider nested equal sign
(define M_value
  (lambda (expression state)
    (if (list? expression)
        (cond
          ((eq? (operator expression) '&&) (myAnd (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '||) (myOr (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '!) (myNot (M_value (operant_1 expression) state)))
          ((eq? (operator expression) '>) (myLarger (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '>=) (myLargerEqual (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '<) (mySmaller (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '<=) (mySmallerEqual (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '==) (eq? (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '!=) (not (eq? (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state)))))
          ((eq? (operator expression) '+) (myAdd (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '-) (if (have_one_operant? expression)    
                                              (mySubtract 0 (M_value (operant_1 expression) state))
                                          (mySubtract (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state)))))
          ((eq? (operator expression) '*) (myMultiply (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '/) (myQuotient (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '%) (myRemainder (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state))))
          ((eq? (operator expression) '=) (M_value (operant_2 expression) state))
          )          
        (cond
          ((eq? expression 'true) #t)
          ((eq? expression 'false) #f)
          ((integer? expression) expression)
          ((not (null? (get_var_value expression state))) (get_var_value expression state))
          (else (error 'expression "Wrong type of expression"))))))

