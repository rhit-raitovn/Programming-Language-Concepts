#lang racket

(require "../chez-init.rkt")
;(provide eval-one-exp)
(require racket/trace)

;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

;DONE
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (id lit?)]
  [quote-exp
   (id quote?)]
  [prim-exp
   (id symbol?)
   (body (list-of expression?))]
  [begin-exp
    (exps (list-of expression?))]
  
;; lambdas
  [lambda-exp
   (id (list-of symbol?))
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
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]

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

(define lit?
  (lambda (x)
    (or (number? x)
            (boolean? x)
            (symbol? x)
            (vector? x)
            (string? x)))
    )

(define quote?
  (lambda (x)
    (if (>= (length x) 1) (eqv? (car x) 'quote) #f)) 
  )

;; check if a list is improper
(define improperlist?
  (lambda (x)
    (and (pair? x) (not (list? x)))))
	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
 

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.


  
;-------------------+
;                   |
;    sec:PARSER     |
;                   |
;-------------------+

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

;DONE
(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(lit? datum) (lit-exp datum)]
      [(quote? datum) (quote-exp datum)]
      [(improperlist? datum) (error 'parse-exp "application ~s is not a proper list" datum)]
      [(not (list? datum)) (error 'parse-exp "application ~s is not a proper list" datum)]
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
         ; BEGIN
         [(eqv? (1st datum) 'begin)
          (cond
            [(< (length datum) 2) (error 'parse-exp "begin-expression does not contain enough values: ~s" datum)]
            [else (begin-exp (map parse-exp (cdr datum)))]
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
            [else (let-exp (map car (2nd datum))
                           (map parse-exp (map cadr (2nd datum)))
                           (map parse-exp (cddr datum)))]
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
               [else (let*-exp (map car (2nd datum))
                           (map parse-exp (map cadr (2nd datum)))
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
            [else (letrec-exp (map car (2nd datum))
                           (map parse-exp (map cadr (2nd datum)))
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

;-------------------+
;                   |
; sec:ENVIRONMENTS  |
;                   |
;-------------------+

; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

;DONE
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (sym (list-of symbol?))   
   (val (list-of scheme-value?))
   (env environment?)])

(define empty-env
  (lambda ()
    (empty-env-record)))

(define init-env
  (lambda ()
    (extend-env *prim-proc-names* (map primitive *prim-proc-names*) (empty-env))))

; got help from tutors
(define apply-env
  (lambda (env sym)
    (cases environment env
      [empty-env-record ()
                        (error 'apply-env "No binding for ~s" sym)]
      [extended-env-record (symbols values env)
                           (let ([index (list-find-pos symbols sym)])
                             (if (number? index)
                                 (list-ref values index)
                                 (apply-env env sym)))]

      )))

; returns index
(define list-find-pos
  (lambda (lst sym)
    (list-find-pos-helper lst sym 0)))

(define list-find-pos-helper
  (lambda (lst sym counter)
    (if (eqv? (car lst) sym)
        counter
        (if (null? (cdr lst))
                   #f
                   (list-find-pos-helper (cdr lst) sym (+ 1 counter))
                   ))))

(define extend-env
 (lambda (syms vals env)
   (extended-env-record syms vals env)))

;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+



;---------------------------------------+
;                                       |
; sec:CONTINUATION DATATYPE and APPLY-K |
;                                       |
;---------------------------------------+

;DONE
(define-datatype closure closure?
  [closure-record
   (ids (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)]
  [primitive
   (prim-symbol symbol?)])

(define global-env init-env)

;-------------------+
;                   |
;  sec:INTERPRETER  |
;                   |
;-------------------+

; DONE
; evaluate that symbol
(define apply-proc
  (lambda (procedure arguments)
    (if (closure? procedure)
	(cases closure procedure
	       [closure-record (ids bodies env)
                               (eval-bodies bodies (extend-env ids arguments env))]
               [primitive (prim-symbol) (apply-prim-proc prim-symbol arguments)])
	(procedure arguments))
    ))



(define apply-prim-proc
  (lambda (prim args)
    (case prim
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (add1 (1st args))]
      [(sub1) (sub1 (1st args))]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(=) (= (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(cons) (cons (1st args) (2nd args))]
      [(car) (1st (1st args))]
      [(cdr) (cdr (1st args))]
      [(list) (apply list args)]
      [(null?) (null? (1st args))]
      [(car) (1st (1st args))]
      [(assq) (assq (1st args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (or (number? (1st args)) (symbol? (1st args)) (procedure? (1st args)))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (or (procedure? (1st args))
                        (closure? (1st args)))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (vector (1st args)(2nd args)(3rd args))]
      [(make-vector) (make-vector (1st args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline (1st args))]
      
      [(car) (car (1st args))]
      [(caar) (caar (1st args))]
      [(caaar) (caaar (1st args))]
      
      [(cdr) (cdr (1st args))]
      [(cadr) (cadr (1st args))]
      [(caddr) (caddr (1st args))]
      
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(cdar) (cdar (1st args))]
      [(cddr) (cddr (1st args))]
      )))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = and < > <= >= cons car cdr list null? 
                              assq eq? equal? atom? length list->vector list? pair? procedure?
                              vector->list vector make-vector vector-ref vector? number? symbol? 
                              vector-set! display newline car caar caaar cdr cadr caadr caadr
                              cadar cdaar cdadr cddar cdddr cdar cddr))

;TODO
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      [lit-exp (value) value]
      [quote-exp (value) (cadr value)]
      ;[var-exp (id) id]
      [var-exp (id) (apply-env env id)]

      ;LAMBDAS
      [lambda-exp (id body) (make-closure id body env)]
      [lambda-list-exp (id body) (make-closure id body env)]
      [lambda-improp-exp (id body) (make-closure id body env)]

      ;IFS
      [if-exp (cond if-true)
              (if (eval-expression cond env)
                  (eval-expression if-true env) #f)]
      [if-else-exp (cond if-true if-false)
              (if (eval-expression cond env)
                  (eval-expression if-true env)
                  (eval-expression if-false env))]

      ;LET
      ;got help from tutors
      [let-exp (vars vals bodies)
               (let ([env (extend-env vars (eval-rands vals env) env)])
                    (eval-bodies bodies env))] 
      [let*-exp (vars vals bodies)
               (let ([env (extend-env vars (eval-rands vals env) env)])
                    (eval-bodies bodies env))]
      [letrec-exp (vars vals bodies)
               (let ([env (extend-env vars (eval-rands vals env) env)])
                    (eval-bodies bodies env))]

      [prim-exp (id body) (make-closure id body env)]
      [set!-exp (id body) (make-closure id body env)]
      [app-exp (procedure argument)
               (let ([proc (eval-expression procedure env)]
                     [arg (eval-rands argument env)])
                 (apply-proc proc arg))]
      ; BEGIN
      [begin-exp (exps) (eval-bodies exps env)]
        )))

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-expression (1st bodies) env)
        (begin (eval-expression (1st bodies) env)
               (eval-bodies (cdr bodies) env)))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define make-closure
 (lambda (id body env)
   (closure-record id body env)))

(define top-level-eval
  (lambda (form)
    (let* ([init-envir (init-env)]
           [result (eval-expression form init-envir)])
      (output-helper result))))

;; Helper function to sanitize closures and primitive procedures
(define output-helper
  (lambda (result)
    (cond
      [(closure? result) '<interpreter-procedure>]
      [(procedure? result) '<interpreter-procedure>]
      [(list? result) (map output-helper result)]  ; recursively sanitize lists
      [else result])))

(define eval-one-exp
  (lambda (x)
    (top-level-eval (parse-exp x))))

(define rep
  (lambda ()
    (display "--> ")
    (let ([input (read)]) ; Read user input
      (cond
        [(and (list? input) (eq? (car input) 'exit))
         (display "Exiting interpreter.\n")
         (void)] ; Exit the interpreter loop
        [else
         (let ([result (top-level-eval (parse-exp input))])
           (pretty-print result)
           (newline)
           (rep))])))) ; Continue the REPL loop


(define eval-one-exp-old
  (lambda (exp)
    (let* ([parse-tree (parse-exp exp)]
	   [initial-environment (init-env)]
           [result (eval-expression (parse-exp exp) initial-environment)])
      (if (or (procedure? result)
                        (closure? result))
          '<interpreter-procedure> result))))


;(trace extend-env)
;(trace eval-one-exp)
;(trace make-closure)
;trace eval-bodies)
;(trace list-find-pos)
;(trace apply-env)


; for begin

