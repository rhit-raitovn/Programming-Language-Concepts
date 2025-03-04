#lang racket

(require "../chez-init.rkt")
(require racket/trace)
(provide bintree-to-list bintree-add leaf-node interior-node parse-exp unparse-exp)


(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

; I've provide this one as a sample to you.
; It's used by the testcases though  so don't mess with it.
(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
      [interior-node (value left right)
                (list value
                      (bintree-to-list left)
                      (bintree-to-list right))]
      [leaf-node (datum)
                 datum])))
                
; Here's the one you need to solve
(define bintree-add
  (lambda (bt num)
    (cond
      [(symbol? (2nd bt))
       (interior-node
        (2nd bt)
        (bintree-add (3rd bt) num)
        (bintree-add (4th bt) num))]
      [else (leaf-node (+ (2nd bt) num))]
      ))) 

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (id lit?)]
  
;; lambdas
  [lambda-exp ;; lambda with single param
   (id symbol?)
   (body (list-of expression?))]
  [lambda-list-exp   ;; lambda with multiple params
   (id (list-of symbol?))
   (body (list-of expression?))
   ]
  [lambda-improp-exp     ;; improper list of params
   (id (list-of improperlist?))
   (body (list-of expression?))
   ]
  
;; lets
  [let-exp
   (id list?)
   (body (list-of expression?))]
  [let*-exp
   (id list?)
   (body (list-of expression?))]
  [letrec-exp
   (id list?)
   (body (list-of expression?))]

;; ifs
  [if-exp
   (cond expression?)
   (if-true expression?)]
  [if-else-exp
   (cond expression?)
   (if-true expression?)
   (if-false expression?)]
  
  [set!-exp
   (id symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand (list-of expression?))]

  )

;; helper function to check if a list satisfies a predicate
(define list-of  
  (lambda (pred)
    (lambda (ls)
      (or (null? ls) (and (pair? ls) (pred (car ls)) ((list-of pred) (cdr ls)) ))
      )
    )
  )

;; check if a list is improper
(define improperlist?
  (lambda (x)
    (and (pair? x) (not (list? x)))))

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(lit? datum) (lit-exp datum)]
      [(improperlist? datum) (error 'parse-exp "application ~s is not a proper list" datum)]
      ;[(not (list? datum)) (error 'parse-exp "application ~s is not a proper list" datum)]
      [(pair? datum)
       (cond
         ;; LAMBDA expressions
         [(eqv? (1st datum) 'lambda) 
          (cond 
            [(< (length datum) 3) (error 'parse-exp "bad expression: ~s" datum)]
            [else 
             (cond
               ;; multiple params
               [(list? (2nd datum)) 
                (if (not (andmap symbol? (2nd datum))) 
                    (error 'parse-exp "lambda argument list must contain symbols only: ~s" (2nd datum))
                    (lambda-list-exp (2nd datum) (map parse-exp (cddr datum)))
                    )]
               ;; single param
               [(symbol? (2nd datum)) (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
               ;; improper list of params
               [(improperlist? (2nd datum)) (lambda-improp-exp (2nd datum)) (map parse-exp (cddr datum))]
               )
             ]
            )]
         
         ;; IF expressions
         [(eqv? (1st datum) 'if)
          (cond
            [(= (length datum) 3) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
            [(= (length datum) 4) (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
            [(> (length datum) 4)  (error 'parse-exp "if-expression contains too many values: ~s" datum)]
            [else  (error 'parse-exp  "if-expression does not contain enough values: ~s" datum)])]

         ;; LET expressions
         [(eqv? (1st datum) 'let) 
          (cond
            [(<= (length datum) 2) (error 'parse-exp "let-expression does not contain enough values: ~s" datum)]
            [(improperlist? (2nd datum)) (error 'parse-exp "let-expression contains an improper list ~s" (2nd datum))]
            [(and (not (eqv? (2nd datum) '()))
                  (ormap improperlist? (2nd datum))) (error 'parse-exp "let-expression contains an improper list ~s" (2nd datum))]
            ;; if the values in let are not length 2
            [(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum)))
             (error 'parse-exp "bindings in let-expression are not all length: ~s" (2nd datum))]
            [(not (andmap symbol? (map car (2nd datum))))
             (error 'parse-exp "bindings in let-expression are not all symbols: ~s" (2nd datum))]				
            [else (let-exp (map parse-id-helper ;; parse each binding
                                (2nd datum)) ;; bindings list
                           (map parse-exp (cddr datum))) ;; parse the body
                  ]
            )]
         
         [(eqv? (1st datum) 'let*)
          (cond
            [(not (list? (2nd datum))) (error 'parse-exp "the 2nd part of the let*-expression must be a list: ~s" datum)]
            [(<= (length datum) 2) (error 'parse-exp "a let*-expresssion must have at least two parts: ~s" datum)]
            [else 
             (cond
               [(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum)))
                (error 'parse-exp "bindings in let*-expression are not all length: ~s" (2nd datum))]
               [(not (andmap symbol? (map car (2nd datum))))
                (error 'parse-exp "bindings in let*-expression are not all symbols: ~s" (2nd datum))]
               [else (let*-exp (map parse-id-helper (2nd datum))
                               (map parse-exp (cddr datum)))]
               )
             ]
            )
          ]

         [(eqv? (1st datum) 'letrec)
          (cond
            [(not (list? (2nd datum))) (error 'parse-exp "the 2nd part of the letrec-expression must be a list: ~s" datum)]
            [(<= (length datum) 2) (error 'parse-exp "a letrec-expresssion must have at least two parts: ~s" datum)]
            [(not (andmap
                   (lambda (ls) (eqv? (length ls) 2)) (2nd datum)))
             (error 'parse-exp "bindings in let*-expression are not all length: ~s" (2nd datum))]
            [(not (andmap symbol? (map car (2nd datum))))
             (error 'parse-exp "bindings in let*-expression are not all symbols: ~s" (2nd datum))]
            [else (letrec-exp (map parse-id-helper (2nd datum))
                              (map parse-exp (cddr datum)))]
               )
             ]

         ;; SET expression
         [(eqv? (1st datum) 'set!)
          (cond
            [(= (length datum) 3) (set!-exp (2nd datum) (parse-exp (3rd datum)))]
            [else (error 'parse-exp "set!-expression must contain three parts only: ~s" datum)])]

         ;; APP expression
         [else (app-exp (parse-exp (1st datum))
                        (map parse-exp (cdr datum)))])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))

;;(trace parse-exp)

;; UNPARSE
(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (datum) datum]
      [lit-exp (id) id]
      [lambda-exp (id body)
                  (append (list 'lambda id) (map unparse-exp body))]
      [lambda-list-exp (id body)
                  (append (list 'lambda id) (map unparse-exp body))]
      [lambda-improp-exp (id body)
                  (list 'lambda id (map unparse-exp body))]
      [app-exp (rator rand)
               (cons (unparse-exp rator) (map unparse-exp rand))]
      [if-exp (cond if-true) (list 'if
                                                  (unparse-exp cond)
                                                  (unparse-exp if-true))]
      [if-else-exp (cond if-true if-false) (list 'if
                                                  (unparse-exp cond)
                                                  (unparse-exp if-true)
                                                  (unparse-exp if-false))]
      [set!-exp (id body)
            (list id (unparse-exp body))]
      [let-exp (id body)
               (append (list 'let (map unparse-id-helper id))
                       (map unparse-exp body))]
      [let*-exp (id body)
               (append (list 'let* (map unparse-id-helper id))
                       (map unparse-exp body))]
      [letrec-exp (id body)
               (append (list 'letrec (map unparse-id-helper id))
                       (map unparse-exp body))])))    

;;(trace unparse-exp)

; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

(define unparse-id-helper
  (lambda (x) 
    (list (1st x) (unparse-exp (2nd x)))
    )
  )

(define parse-id-helper
  (lambda (x) 
    (list (1st x) (parse-exp (2nd x)))
    )
  )

(define lit?
  (lambda (x)
    (or (number? x)
        (boolean? x)
        (symbol? x)
        (vector? x)
        (string? x))
    )
  )

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
