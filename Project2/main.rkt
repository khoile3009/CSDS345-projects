#lang racket
(require "simpleParser.rkt")

(define popFrame
  (lambda (state)
    (cdr state)))

(define pushFrame
  (lambda (state)
    (cons '() state)))



;expression abstraction
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

;check if there is an else statement
(define have_operant_3
  (lambda (expression)
    (not (null?(cdddr expression)))))

;helper function to get the value of a pair
(define pair_value
  (lambda (pair)
    (car (cdr pair))))

;get value of a variable
(define get_var_value
  (lambda (expression state)
    (cond
      ((null? state) '())
      ((null? (get_var_value_frame expression (car state))) (get_var_value expression (cdr state)))
      (else (get_var_value_frame expression (car state))))))

(define get_var_value_frame
  (lambda (expression frame)
    (cond
      ((null? frame) '())
      ((eq? expression (car (car frame))) (pair_value (car frame)))
      (else (get_var_value_frame expression (cdr frame))))))

;interpret and
(define myAnd
  (lambda (op1 op2)
    (if (and (boolean? op1) (boolean? op2))
        (and op1 op2)
        (error 'op "Both operant need to be boolean"))))

;interpret or
(define myOr
  (lambda (op1 op2)
    (if (and (boolean? op1) (boolean? op2))
        (or op1 op2)
        (error 'op "Both operant need to be boolean"))))

;interpret not
(define myNot
  (lambda (op)
    (if (boolean? op)
        (not op)
        (error 'op "Both operant need to be boolean"))))

;interpret >
(define myLarger
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (> op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret >=
(define myLargerEqual
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (>= op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret <
(define mySmaller
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (< op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret <=
(define mySmallerEqual
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (<= op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret +
(define myAdd
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (+ op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret -
(define mySubtract
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (- op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret /
(define myQuotient
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (quotient op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret *
(define myMultiply
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (* op1 op2)
        (error 'op "Both operant need to be integer"))))

;intepret %
(define myRemainder
  (lambda (op1 op2)
    (if (and (integer? op1) (integer? op2))
        (modulo op1 op2)
        (error 'op "Both operant need to be integer"))))

;interpret =
(define myAssign
  (lambda (var value state)
    (if (initialized? var state)
        (updateState var value state)
        (error 'var "variable is not initialized"))))

;interpret if
(define myIf
  (lambda (expression state break continue err)
    (cond
      ((not (boolean? (M_value (operant_1 expression) state break continue err))) (error 'expression "condition should be boolean"))
      ((M_value (operant_1 expression) state break continue err) (M_state (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err))
      ((not (have_operant_3 expression)) state)
      (else (M_state (operant_3 expression) (M_state (operant_1 expression) state break continue err) break continue err)))))

;interpret while
(define myWhile
  (lambda (expression state break continue err)
    (cond
      ((M_value (operant_1 expression) state break continue err) (myWhile expression (call/cc (lambda (newContinue) (M_state (operant_2 expression) (M_state (operant_1 expression) state break continue err) break newContinue err))) break continue err))
      (else (M_state (operant_1 expression) state break continue err)))))


;check if expression have one operant
(define have_one_operant?
  (lambda (expression)
    (null? (cddr expression))))

;create pair of key and value
(define toKeyValuePair
  (lambda (key value)
    (cons key (cons value '()))))

;add pair of key and value to state
(define addToState
  (lambda (var value state)
    (cons (cons (toKeyValuePair var value) (car state)) (cdr state))))

;check if variable has been intitialized
(define initialized?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((initializedFrame? var (car state)) #t)
      (else (initialized? var (cdr state))))))

(define initializedFrame?
  (lambda (var frame)
    (cond
      ((null? frame) #f)
      ((eq? var (car (car frame))) #t)
      (else (initializedFrame? var (cdr frame))))))

;update state
(define updateState
  (lambda (var value state)
    (if (null? state) '()
        (cons (updateStateFrame var value (car state)) (updateState var value (cdr state))))))

(define updateStateFrame
  (lambda (var value frame)
    (cond
      ((null? frame) '())
      ((eq? var (car (car frame))) (cons (toKeyValuePair var value) (cdr frame)))
      (else (cons (car frame) (updateStateFrame var value (cdr frame)))))))

;initialize variable with its value
(define myInitialize
  (lambda (var value state)
    (if (initialized? var state)
        (error 'var "variable is initialized")
        (addToState var value state))))

;This traverse through the statements and build up the state 
(define traverseStatements
  (lambda (statements state break continue err)
    (if (null? statements)
        state
        (traverseStatements (cdr statements) (M_state (car statements) state break continue err) break continue err))))

;Parse resulting value into java-like format. Only accept int or bool
(define parse_value
  (lambda (value)
    (cond
      ((boolean? value) (if value 'true 'false))
      ((integer? value) value)
      (else (error 'value "Value should be int or boolean")))))

(define noLoopError
  (lambda ()
    (error "No loop to break")))

(define errorWithMessage
  (lambda (msg)
    (error msg)))

;This method take in a filename, parse it into statements and traverse the state. Then return what is stored in the return variable.
;TODO(Khoi): This probably need to refactor so that return can be called in a block and not got pop out of the stack
(define runFile
  (lambda (filename)
    (parse_value (get_var_value 'return (traverseStatements (parser filename) '(()) noLoopError noLoopError errorWithMessage)))))


;Apply M_state of operant 1 to M_state of operant 2
(define 2_operants_M_state
  (lambda (expression state break continue err)
    (if (have_one_operant? expression)
        (error 'expression "have only one operant")
        (M_state (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err))))

;M_state operation, this also consider nested equal sign
(define M_state
  (lambda (expression state break continue err)
    (cond
      ((not (list? expression)) state)
      ((eq? (operator expression) 'var) (if (have_one_operant? expression)
                                            (myInitialize (operant_1 expression) '() state)
                                            (myInitialize (operant_1 expression) (M_value (operant_2 expression) state break continue err) (M_state (operant_2 expression) state break continue err))))
      ((eq? (operator expression) '=) (myAssign (operant_1 expression) (M_value (operant_2 expression) state break continue err) (M_state (operant_2 expression) state break continue err)))
      ((eq? (operator expression) 'return) (myInitialize 'return (M_value (operant_1 expression) state break continue err) (M_state(operant_1 expression) state break continue err)))
      ((eq? (operator expression) 'if) (myIf expression state break continue err))
      ((eq? (operator expression) 'while) (call/cc (lambda (newBreak)
                                                   (myWhile expression state newBreak continue err))))
      ((eq? (operator expression) '&&) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '||) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '!) (M_state (operant_1 expression) state break continue err))
      ((eq? (operator expression) '>) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '>=) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '<) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '<=) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '==) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '!=) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '+) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '-) (if (have_one_operant? expression)
                                          (M_state (operant_1 expression) state break continue err)
                                          (2_operants_M_state expression state break continue err)))
      ((eq? (operator expression) '*) (2_operants_M_state expression state break continue err))
      ((eq? (operator expression) '/) (2_operants_M_state expression state  break continue err))
      ((eq? (operator expression) '%) (2_operants_M_state expression state  break continue err))
      ((eq? (operator expression) 'begin) (popFrame (traverseStatements (cdr expression) (pushFrame state) break continue err)))
      ((eq? (operator expression) 'break) (break (popFrame state))) ;What if there is only 1 element and no new frame
      ((eq? (operator expression) 'continue) (continue (popFrame state)))
      (else state)
      )))

;M_value operation, this also consider nested equal sign
(define M_value
  (lambda (expression state break continue err)
    (if (list? expression)
        (cond
          ((eq? (operator expression) '&&) (myAnd (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err )))
          ((eq? (operator expression) '||) (myOr (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '!) (myNot (M_value (operant_1 expression) state break continue err)))
          ((eq? (operator expression) '>) (myLarger (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '>=) (myLargerEqual (M_value (operant_1 expression) state) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '<) (mySmaller (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '<=) (mySmallerEqual (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '==) (eq? (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '!=) (not (eq? (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err))))
          ((eq? (operator expression) '+) (myAdd (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '-) (if (have_one_operant? expression)    
                                              (mySubtract 0 (M_value (operant_1 expression) state break continue err))
                                          (mySubtract (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err))))
          ((eq? (operator expression) '*) (myMultiply (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '/) (myQuotient (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '%) (myRemainder (M_value (operant_1 expression) state break continue err) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue err) break continue err)))
          ((eq? (operator expression) '=) (M_value (operant_2 expression) state break continue err))
          )          
        (cond
          ((eq? expression 'true) #t)
          ((eq? expression 'false) #f)
          ((integer? expression) expression)
          ((not (null? (get_var_value expression state))) (get_var_value expression state))
          (else (error 'expression "Wrong type of expression"))))))

