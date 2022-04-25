#lang racket
(require "functionParser.rkt")

;remove a frame from state
(define popFrame
  (lambda (state)
    (cdr state)))

;add a frame to the top of state
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

;check if there exists a third operand in a statement
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

;get variable of a frame
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
    (if (initialized*? var state)
        (updateState var value state)
        (error 'var "variable is not initialized"))))

;update state with variable and value
(define updateState
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((null? (car state)) (cons '() (updateState var value (cdr state))))
      ((eq? var (car (car (car state)))) (cons (cons (toKeyValuePair var value) (cdr (car state))) (cdr state)))
      (else (let* ((updatedFollowing (updateState var value (cons (cdr (car state)) (cdr state)))))
              (cons (cons (car (car state)) (car updatedFollowing)) (cdr updatedFollowing)))))))

;interpret if
(define myIf
  (lambda (expression state break continue throw return)
    (cond
      ((not (boolean? (M_value (operant_1 expression) state break continue throw return))) (error 'expression "condition should be boolean"))
      ((M_value (operant_1 expression) state break continue throw return) (M_state (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return))
      ((not (have_operant_3 expression)) state)
      (else (M_state (operant_3 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))))

;interpret while
(define myWhile
  (lambda (expression state break continue throw return)
    (cond
      ((M_value (operant_1 expression) state break continue throw return) (myWhile expression (call/cc (lambda (newContinue) (M_state (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break newContinue throw return))) break continue throw return))
      (else (M_state (operant_1 expression) state break continue throw return)))))


;helper function to define new throw
(define newThrowCatchHandler
  (lambda (catchStatement state break continue throw return jump finallyBlock)
    (cond 
      ((null? catchStatement) (lambda (ex env) (throw ex (traverseStatements finallyBlock env break continue throw return))))
      ((not (eq? 'catch (operator catchStatement))) (error "Incorrect catch statement"))
      (else (lambda (ex s)
              (jump (traverseStatements finallyBlock
                                        (popFrame (traverseStatements
                                                   (operant_2 catchStatement)
                                                   (myInitialize (catchVar catchStatement) ex (pushFrame s))
                                                   (lambda (s1) (break (popFrame s1)))
                                                   (lambda (s1) (continue (popFrame s1)))
                                                   (lambda (v s1) (throw v (popFrame s1)))
                                                   return))
                                        break continue throw return)))))))
                                                        
(define getCatchStatement operant_2)
(define catchVar (lambda (catchStatement) (car (operant_1 catchStatement))))

;interpret try
(define myTry
  (lambda (expression state break continue throw return)
    (call/cc
     (lambda (jump)
       (let* ((finallyBlock (getFinallyBlock (getFinallyStatement expression)))
              (tryBlock (operant_1 expression))
              (newReturn (lambda (env) (return (traverseStatements finallyBlock state break continue throw return))))
              (newBreak (lambda (env) (break (traverseStatements finallyBlock state break continue throw return))))
              (newContinue (lambda (env) (continue (traverseStatements finallyBlock state break continue throw return))))
              (newThrow (newThrowCatchHandler (getCatchStatement expression) state break continue throw return jump finallyBlock)))
         (traverseStatements finallyBlock
                             (traverseStatements tryBlock state newBreak newContinue newThrow newReturn)
                             break continue throw return))))))

(define statementType operator)

(define getFinallyStatement operant_3)

;get the finally block
(define getFinallyBlock
  (lambda (finallyStatement)
    (cond
      ((null? finallyStatement) '() )
      ((not (eq? (statementType finallyStatement) 'finally)) (error "Wrong finally format"))
      (else (cadr finallyStatement)))))

;check if expression have one operant
(define have_one_operant?
  (lambda (expression)
    (null? (cddr expression))))

;create pair of key and value
(define toKeyValuePair
  (lambda (key value)
    (list key value)))


;add pair of key and value to state
(define addToState
  (lambda (var value state)
    (cons (cons (toKeyValuePair var value) (car state)) (cdr state))))

;check if variable has been intitialized first frame
(define initialized?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((initializedFrame? var (car state)) #t)
      (else #f))))

;initialized on all frame
(define initialized*?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((initializedFrame? var (car state)) #t)
      (else (initialized*? var (cdr state))))))

;check if variable in a frame has been initialized
(define initializedFrame?
  (lambda (var frame)
    (cond
      ((null? frame) #f)
      ((eq? var (car (car frame))) #t)
      (else (initializedFrame? var (cdr frame))))))

;initialize variable with its value
(define myInitialize
  (lambda (var value state)
    (if (initialized? var state)
        (error 'var "variable is initialized")
        (addToState var value state))))

;This traverse through the statements and build up the state 
(define traverseStatements
  (lambda (statements state break continue throw return)
    (if (null? statements)
        state
        (traverseStatements (cdr statements) (M_state (car statements) state break continue throw return) break continue throw return))))

;Parse resulting value into java-like format. Only accept int or bool
(define parse_value
  (lambda (value)
    (cond
      ((boolean? value) (if value 'true 'false))
      ((integer? value) value)
      (else (error 'value "Value should be int or boolean")))))

;helper function defines no loop error
(define noLoopError
  (lambda (state)
    (error "No loop to break")))

;helper function defines default error
(define defaultError
  (lambda (ex state)
    (error "Error of: " ex)))

;get value of main function
(define getMainFunction
  (lambda (state)
    (if (get_var_value 'main state)
        (get_var_value 'main state)
        (error 'state "no main function defined"))))

;helper function defines default return
(define defaultReturn
  (lambda (state)
    (error 'state "No function to return")))

;This method take in a filename, parse it into statements and traverse the state. Then return what is stored in the return variable.
(define runFile
  (lambda (filename)
    (let* ((state  (traverseStatements (parser filename) '(()) noLoopError noLoopError defaultError defaultReturn))
           (mainFunc (getMainFunction state))
           (statements (cadr mainFunc))
           (newState (pushFrame state)))
      (parse_value  (get_var_value 'return (call/cc (lambda (newReturn) (traverseStatements statements newState noLoopError noLoopError defaultError newReturn))))))))


;Apply M_state of operant 1 to M_state of operant 2
(define 2_operants_M_state
  (lambda (expression state break continue throw return)
    (if (have_one_operant? expression)
        (error 'expression "have only one operant")
        (M_state (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return))))

;helper function to get name, body, params of a function
(define getFunctionName operant_1)
(define getFuntionBody operant_3)
(define getFunctionParams operant_2)

;helper function to get params, statements, scope from a function
(define getParamsFromFunctionDefinition car)
(define getStatementsFromFunctionDefinition cadr)
(define getScopeFromFunctionDefinition caddr)

;interpret function definition and add it to state 
(define addFunctionToState
  (lambda (expression state)
    (addToState (getFunctionName expression) (getFunctionDefinition expression) state)))

;Here function will be defined as a tuple of inputs and list of statements
(define getFunctionDefinition
  (lambda (expression)
    (list (getFunctionParams expression) (getFuntionBody expression))))

;get params of function call
(define getFunctionCallParams
  (lambda (expression)
    (cddr expression)))

;update state after function call
(define updateStateAfterFunction
  (lambda (fname scope state)
    (cond
      ((null? state) '())
      ((null? (car state)) (cons '() (updateStateAfterFunction fname scope (cdr state))))
      ((eq? fname (car (car (car state)))) scope)
      (else (let* ((updatedState (updateStateAfterFunction fname scope (cons (cdr (car state)) (cdr state)))))
              (cons (cons (car (car state)) (car updatedState)) (cdr updatedState)))))))

;M_value of a list of parameters
(define evaluateParameters
  (lambda (params state break continue throw return)
    (if (null? params)
        '()
        (cons (M_value (car params) state break continue throw return) (evaluateParameters (cdr params) state break continue throw return)))))

;make a new state with actual parameters
(define addParamsToState
  (lambda (params values state)
    (cond
      ((and (null? params) (null? values)) state)
      ((or (null? params) (null? values)) (error 'params "params and values have different size"))
      (else (addParamsToState (cdr params) (cdr values) (addToState (car params) (car values) state))))))

;get a function from a state
(define getFunctionFromState
  (lambda (fname state)
    (if (null? (get_var_value fname state))
        (error 'fname "function not defined")
        (getFunctionDefinitionAndScope fname state))))

(define convertToFunctionDefinition
  (lambda (func state)
    (append func (list state))))

(define getFunctionDefinitionAndScope
  (lambda (fname state)
    (cond
      ((null? state) '())
      ((null? (car state)) (getFunctionDefinitionAndScope fname (cdr state)))
      ((eq? fname (car (car (car state)))) (convertToFunctionDefinition (pair_value (car (car state))) state))
      (else (getFunctionDefinitionAndScope fname (cons (cdr (car state)) (cdr state)))))))

;process a function call and return a value with state
(define callFunction
  (lambda (expression state break continue throw return)
    (let* ((fname (getFunctionName expression))
           (func (getFunctionFromState fname state))
           (params (getParamsFromFunctionDefinition func))
           (statements (getStatementsFromFunctionDefinition func))
           (scope (getScopeFromFunctionDefinition func))
           (paramNames (getFunctionCallParams expression))
           (paramValues (evaluateParameters paramNames state break continue throw return))
           (newState (addParamsToState params paramValues (pushFrame scope)))
           (newBreak (lambda (s1) (break (updateStateAfterFunction fname (popFrame s1) state))))
           (newContinue (lambda (s1) (continue (updateStateAfterFunction fname (popFrame s1) state))))
           (newThrow (lambda (ex s1) (throw ex (updateStateAfterFunction fname (popFrame s1) state))))
           (returnedState (call/cc (lambda (newReturn) (traverseStatements statements newState newBreak newContinue newThrow newReturn))))
           (returnValue (get_var_value 'return returnedState))
           (mergedState (updateStateAfterFunction fname (popFrame returnedState) state)))
      (list returnValue mergedState))))

;M_state of a function call
(define M_state_function
  (lambda (expression state break continue throw return)
    (cadr (callFunction expression state break continue throw return))))

;M_value of a function call
(define M_value_function
  (lambda (expression state break continue throw return)
    (car (callFunction expression state break continue throw return))))
           
;M_state operation, this also consider nested equal sign
(define M_state
  (lambda (expression state break continue throw return)
    (cond
      ((not (list? expression)) state)
      ((eq? (operator expression) 'var) (if (have_one_operant? expression)
                                            (myInitialize (operant_1 expression) '() state)
                                            (myInitialize (operant_1 expression) (M_value (operant_2 expression) state break continue throw return) (M_state (operant_2 expression) state break continue throw return))))
      ((eq? (operator expression) '=) (myAssign (operant_1 expression) (M_value (operant_2 expression) state break continue throw return) (M_state (operant_2 expression) state break continue throw return)))
      ((eq? (operator expression) 'return) (return (myInitialize 'return (M_value (operant_1 expression) state break continue throw return) (M_state(operant_1 expression) state break continue throw return))))
      ((eq? (operator expression) 'if) (myIf expression state break continue throw return))
      ((eq? (operator expression) 'while) (call/cc (lambda (newBreak)
                                                     (myWhile expression state newBreak continue throw return))))
      ((eq? (operator expression) '&&) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '||) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '!) (M_state (operant_1 expression) state break continue throw return))
      ((eq? (operator expression) '>) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '>=) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '<) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '<=) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '==) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '!=) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '+) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '-) (if (have_one_operant? expression)
                                          (M_state (operant_1 expression) state break continue throw return)
                                          (2_operants_M_state expression state break continue throw return)))
      ((eq? (operator expression) '*) (2_operants_M_state expression state break continue throw return))
      ((eq? (operator expression) '/) (2_operants_M_state expression state  break continue throw return))
      ((eq? (operator expression) '%) (2_operants_M_state expression state  break continue throw return))
      ((eq? (operator expression) 'begin) (popFrame
                                           (traverseStatements
                                            (cdr expression)
                                            (pushFrame state)
                                            (lambda (state) (break (popFrame state)))
                                            (lambda (state) (continue (popFrame state)))
                                            throw
                                            return)))
      ((eq? (operator expression) 'break) (break state))
      ((eq? (operator expression) 'continue) (continue state))
      ((eq? (operator expression) 'try) (myTry expression state break continue throw return))
      ((eq? (operator expression) 'throw) (throw (M_value (operant_1 expression) state break continue throw return) state))
      ((eq? (operator expression) 'function) (addFunctionToState expression state))
      ((eq? (operator expression) 'funcall) (M_state_function expression state break continue throw return))
      (else state)
      )))

;M_value operation, this also consider nested equal sign
(define M_value
  (lambda (expression state break continue throw return)
    (if (list? expression)
        (cond
          ((eq? (operator expression) '&&) (myAnd (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return )))
          ((eq? (operator expression) '||) (myOr (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '!) (myNot (M_value (operant_1 expression) state break continue throw return)))
          ((eq? (operator expression) '>) (myLarger (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '>=) (myLargerEqual (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '<) (mySmaller (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '<=) (mySmallerEqual (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '==) (eq? (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '!=) (not (eq? (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return))))
          ((eq? (operator expression) '+) (myAdd (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '-) (if (have_one_operant? expression)    
                                              (mySubtract 0 (M_value (operant_1 expression) state break continue throw return))
                                              (mySubtract (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return))))
          ((eq? (operator expression) '*) (myMultiply (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '/) (myQuotient (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '%) (myRemainder (M_value (operant_1 expression) state break continue throw return) (M_value (operant_2 expression) (M_state (operant_1 expression) state break continue throw return) break continue throw return)))
          ((eq? (operator expression) '=) (M_value (operant_2 expression) state break continue throw return))
          ((eq? (operator expression) 'funcall) (M_value_function expression state break continue throw return))
          )          
        (cond
          ((eq? expression 'true) #t)
          ((eq? expression 'false) #f)
          ((integer? expression) expression)
          ((not (null? (get_var_value expression state))) (get_var_value expression state))
          (else (error 'expression "Wrong type of expression"))))))

