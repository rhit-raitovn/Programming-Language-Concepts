#lang racket

(require "../chez-init.rkt")
(provide top-level-eval eval-one-exp)
(require racket/trace)

;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

(define (quoted? exp)
  (and (pair? exp)
       (eq? (car exp) 'quote)))

(define literal?
  (lambda (exp)
    (or
     (void? exp)
     (number? exp)
     (boolean? exp)
     (string? exp)
     (vector? exp))))

(define proper-list?
  (lambda (lst)
    (if (null? lst)
        #t
        (and (pair? lst) (proper-list? (cdr lst))))))

(define nullablePair?
  (lambda (lst)
    (or (pair? lst) (null? lst))))

; parsed expression.  You'll probably want to replace this 
; code with your expression datatype from A11b

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data literal?)]
  [quoted-exp
   (data quoted?)]
  [lambda-exp
   (ids (lambda (x) (or (symbol? x) (nullablePair? x))))
   (bodies pair?)]
  [app-exp
   (rator expression?)
   (rands nullablePair?)]
  [let-exp
   (ids nullablePair?)
   (bodies pair?)]
  [letrec-exp
   (ids nullablePair?)
   (bodies pair?)]
  [let*-exp
   (ids nullablePair?)
   (bodies pair?)]
  [named-let-exp
   (name symbol?)
   (ids nullablePair?)
   (bodies pair?)]
  [if-exp
   (conditional expression?)
   (if-true expression?)]
  [if-else-exp
   (conditional expression?)
   (if-true expression?)
   (if-false expression?)]
  [define-exp
    (id symbol?)
    (value expression?)]
  [set-bang-exp
   (id symbol?)
   (value expression?)]
  [begin-exp
    (bodies (listof expression?))]
  [or-exp
   (conditions (listof expression?))]
  [and-exp
   (conditions (listof expression?))]
  [cond-exp
   (clauses (listof pair?))]
  [case-exp
   (id expression?)
   (clauses (listof pair?))])
	

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

; This is a parser for simple Scheme expressions, such as those in EOPL 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Helper procedures to make the parser a little bit saner.
(define valid-lambda-args?
  (lambda (lst)
    (if (or (symbol? lst) (null? lst))
        #t
        (and (symbol? (car lst)) (valid-lambda-args? (cdr lst))))))

(define correct-let-args
  (lambda (lst)
    (if (null? lst)
        #t
        (let ([lst (car lst)] [o lst])
          (and (pair? lst) (proper-list? lst) (= (length lst) 2) (symbol? (car lst)) (correct-let-args (cdr o)))))))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(quoted? datum) (quoted-exp datum)]
      [(literal? datum) (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'lambda)
          (if (or (null? (cddr datum)) (not (valid-lambda-args? (2nd datum))))
              (error 'parse-exp "bad expression: ~s" datum)
              (lambda-exp
               (2nd datum)
               (map
                parse-exp
                (cddr datum))))]

         [(eqv? (1st datum) 'let)
          (if (and (or (null? (2nd datum)) (pair? (2nd datum))) (proper-list? (2nd datum)))
              (if (or (null? (cdr datum)) (null? (cddr datum)) (not (correct-let-args (2nd datum))))
                  (error 'parse-exp "bad expression: ~s" datum)        
                  (cond
                    [(nullablePair? (2nd datum))
                     (let-exp (map
                               (lambda (id)
                                 (list (car id) (parse-exp (cadr id))))
                               (2nd datum))
                              (map
                               parse-exp
                               (cddr datum)))]
                    [(symbol? (cadr datum))
                     (named-let-exp
                      (2nd datum)
                      (map
                       (lambda (id)
                         (list (car id) (parse-exp (cadr id))))
                       (3rd  datum))
                      (map
                       parse-exp
                       (cdddr datum)))]
                    [else (error 'parse-exp "bad expression: ~s" datum)]))
              (error 'parse-exp "bad expression: ~s" datum))]
                      
         [(eqv? (car datum) 'let*)
          (if (and (or (null? (2nd datum)) (pair? (2nd datum))) (proper-list? (2nd datum)))
              (if (or (null? (cdr datum)) (null? (cddr datum)) (not (correct-let-args (2nd datum))))
                  (error 'parse-exp "bad expression: ~s" datum)
                  (let*-exp (map
                             (lambda (id)
                               (list (car id) (parse-exp (cadr id))))
                             (cadr datum))
                            (map
                             parse-exp
                             (cddr datum))))
              (error 'parse-exp "bad expression: ~s" datum))]
         [(eqv? (car datum) 'letrec)
          (letrec-exp (map
                       (lambda (id)
                         (list (car id) (parse-exp (cadr id))))
                       (cadr datum))
                      (map
                       parse-exp
                       (cddr datum)))]
          ;(if (and (or (null? (2nd datum)) (pair? (2nd datum))) (proper-list? (2nd datum)))
           ;   (if (or (null? (cdr datum)) (null? (cddr datum)) (not (correct-let-args (2nd datum))))
            ;      (error 'parse-exp "bad expression: ~s" datum)
             ;     (letrec-exp (map
              ;                 (lambda (id)
               ;                  (list (car id) (parse-exp (cadr id))))
                ;               (cadr datum))
                 ;             (map
                  ;             parse-exp
                   ;            (cddr datum))))
             ; (error 'parse-exp "bad expression: ~s" datum))]

         [(eqv? (car datum) 'if)
          (cond
            [(= (length datum) 3)
             (if-exp
              (parse-exp (2nd datum))
              (parse-exp (3rd datum)))]
            [(= (length datum) 4)
             (if-else-exp
              (parse-exp (2nd datum))
              (parse-exp (3rd datum))
              (parse-exp (4th datum)))])]

         [(eqv? (car datum) 'or)
          (if (null? (cdr datum))
              (lit-exp #f)
              (or-exp (map parse-exp (cdr datum))))]

         [(eqv? (car datum) 'and)
          (if (null? (cdr datum))
              (lit-exp #t)
              (and-exp (map parse-exp (cdr datum))))]

         [(eqv? (car datum) 'cond)
          (cond-exp (map 
                     (lambda (clause)
                       (map parse-exp clause))
                     (cdr datum)))]

         [(eqv? (car datum) 'case)
          (case-exp
           (parse-exp (cadr datum))
           (map
            (lambda (clause)
              (cons
               (car clause)  ; keep literals as raw data
               (map parse-exp (cdr clause))))
            (cddr datum)))]

         [(eqv? (car datum) 'define)
          (define-exp
            (2nd datum)
            (parse-exp (3rd datum)))]
         
         [(eqv? (car datum) 'set!)
          (if (= (length (cdr datum)) 2)
              (set-bang-exp
               (2nd datum)
               (parse-exp (3rd datum)))
              (error 'parse-exp "bad expression: ~s" datum))]

         [(eqv? (car datum) 'begin)
          (if (null? (cdr datum))
              (lit-exp (void))
              (begin-exp
                (map parse-exp (cdr datum))))]
                
         [else
          (if (and (nullablePair? (cdr datum)) (proper-list? (cdr datum)))
              (app-exp (parse-exp (1st datum))
                       (map
                        (lambda (e) (parse-exp e))
                        (cdr datum)))
              (error 'parse-exp "bad expression: ~s" datum))])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))


;-------------------+
;                   |
; sec:ENVIRONMENTS  |
;                   |
;-------------------+


; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

;;; Environment

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (listof symbol?))   
   (vals vector?)
   (env environment?)])

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;(trace extend-env)

(define apply-env
  (lambda (env sym)
    (cases environment env
      [empty-env-record ()
                        (if (hash-has-key? global-env sym)
                            (hash-ref global-env sym)
                            (error 'apply-env "No binding for ~s" sym))]
      [extended-env-record (symbols values env)
                           (let ([pos (find-position sym symbols 0)])
                             (if (number? pos)
                                 (vector-ref values pos)
                                 (apply-env env sym)))])))


(define redefine-variable
  (lambda (env sym value)
    (cases environment env
      [empty-env-record ()
                        (if (hash-has-key? global-env sym)
                            (hash-set! global-env sym value)
                            (error 'redefine-variable "Temporary error - can't set before defining (for ~s)" sym))]
      [extended-env-record (symbols values env)
                           (let ([pos (find-position sym symbols 0)])
                             (if (number? pos)
                                 (vector-set! values pos value)
                                 (redefine-variable env sym value)))])))

(define temp-env-vector
  (lambda (size)
    (make-vector size '_systemname)))

;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+

;;; Syntax-expand

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [lit-exp (_) exp]
      [var-exp (_) exp]
      [quoted-exp (_) exp]
      [letrec-exp (_ __) exp]
      [named-let-exp (_ __ ___) exp]
      [if-exp (_ __) exp]
      [if-else-exp (_ __ ___) exp]
      [define-exp (_ __) exp]
      [set-bang-exp (_ __) exp]
      [begin-exp (bodies) (syntax-expand (let-exp '() bodies))]
      [let-exp (ids bodies)
               (let [(syms (map car ids)) (vals (map cadr ids))]
                 (app-exp (lambda-exp syms (map syntax-expand bodies)) (map syntax-expand vals)))]
      [let*-exp (ids bodies)
                 (let helper-let*-expand ([ids ids])
                   (if (null? ids)
                       (syntax-expand
                        (let-exp '() (map syntax-expand bodies)))
                       (syntax-expand
                        (let-exp
                         (list (car ids))
                         (list (helper-let*-expand (cdr ids)))))))]
      [lambda-exp (ids bodies) (lambda-exp ids (map syntax-expand bodies))]

      [or-exp (conditions)
              (if (null? (cdr conditions))
                  (car conditions)
                  (if-else-exp
                   (car conditions)
                   (car conditions)   ; returns itselkf if true
                   (syntax-expand (or-exp (cdr conditions)))))]

      [and-exp (conditions)
               (if (null? (cdr conditions))
                   (car conditions)
                   (if-else-exp
                    (car conditions)
                    (syntax-expand (and-exp (cdr conditions)))
                    (lit-exp #f)))]

      [cond-exp (clauses)
                (if (null? clauses)
                    (lit-exp (void))
                    (let ([clause (1st clauses)])
                      (let ([first-value (1st clause)])
                        (cases expression first-value
                          [var-exp (id)
                                   (if (eq? id 'else)
                                       (syntax-expand (begin-exp (cdr clause)))
                                       (if-else-exp
                                        (syntax-expand first-value)
                                        (syntax-expand (begin-exp (cdr clause)))
                                        (syntax-expand (cond-exp (cdr clauses)))))]
                          [else
                           (if-else-exp
                                        (syntax-expand first-value)
                                        (syntax-expand (begin-exp (cdr clause)))
                                        (syntax-expand (cond-exp (cdr clauses))))]))))]

      [case-exp (id clauses)
                (if (null? clauses)
                    (lit-exp (void))
                    (let ([clause (1st clauses)])
                      (if (eq? (car clause) 'else)
                          (syntax-expand (begin-exp (cdr clause)))
                          (if-else-exp
                           (or-exp
                            (map (lambda (elem)
                                   (app-exp (var-exp 'eq?) (list (lit-exp elem) id)))
                                   (car clause)))
                           (syntax-expand (begin-exp (cdr clause)))
                           (syntax-expand (case-exp id (cdr clauses)))))))]                    
      
      [app-exp (rator rands) (app-exp (syntax-expand rator) (map syntax-expand rands))])))

;(trace syntax-expand)

;---------------------------------------+
;                                       |
; sec:CONTINUATION DATATYPE and APPLY-K |
;                                       |
;---------------------------------------+

; To be added later


;-------------------+
;                   |
;  sec:INTERPRETER  |
;                   |
;-------------------+



(define eval-expression
  (lambda (exp env)
    (cases expression exp
      [lit-exp (value) value]
      [quoted-exp (value) (2nd value)]
      [var-exp (id) (apply-env env id)]
      [lambda-exp (ids body) (make-closure ids body env)]
      [if-else-exp (conditional if-true if-false)
                   (if (eval-expression conditional env)
                       (eval-expression if-true env)
                       (eval-expression if-false env))]
      [app-exp (rator rands)
               (apply-proc (eval-expression rator env)
                           (map (lambda (x) (eval-expression x env)) rands))]  ; hz
      [begin-exp (bodies)
                 (car (reverse (map (lambda (exp) (eval-expression exp env)) bodies)))]
      [set-bang-exp (id value)
                    ;(display 'hello)
                    ;(newline)
                    (redefine-variable env id (eval-expression value env))]
      [define-exp (id value)
                  (add-to-global-env id (eval-expression value env))]
      [letrec-exp (ids bodies)
                  (let* [(syms (map 1st ids))
                         (vals (map 2nd ids))
                         (new-env (extend-env syms vals env))]
                    (for-each
                     (lambda (sym val)
                       (redefine-variable new-env sym (eval-expression val new-env)))
                     syms
                     vals)

                    (car (reverse (map (lambda (exp) (eval-expression exp new-env)) bodies))))]             
      
      [else (error 'eval-expression "Not syntax-expanded: ~s" exp)])))

(define make-closure
  (lambda (ids body env)
    (closure-record ids body env)))

;(trace make-closure)

(define-datatype closure closure?
  [primitive
   (id symbol?)]
  [closure-record
   (id (listof symbol?))
   (body nullablePair?)
   (env environment?)])

;; extend an env
;; evaluate body of closure

(define apply-proc
  (lambda (procedure arguments)
    (if (closure? procedure)
        (cases closure procedure
          [primitive (id)
                     (apply-prim-proc id arguments)]
          [closure-record (ids body env)
                          ; (eval-expression (car (reverse body)) (extend-env ids arguments env))])
                          (let [(new-env (extend-env ids arguments env))]
                            (car (reverse (map (lambda (b) (eval-expression b new-env)) body))))])
                          ;(map (lambda (b) (eval-expression b (extend-env ids arguments env))) body)])
        (error 'apply-proc "No procedure given ~s" procedure))))

;(trace apply-proc)

(define apply-prim-proc
  (lambda (id arguments)
    (case id
      [(list) arguments]
      [(car) (caar arguments)]
      [(add1) (+ (car arguments) 1)]
      [(-) (apply - arguments)]
      [(+) (apply + arguments)]
      [(*) (apply * arguments)]
      [(/) (apply / arguments)]
      [(sub1) (- (1st arguments) 1)]
      [(zero?) (zero? (1st arguments))]
      [(not) (not (1st arguments))]
      [(=) (apply = arguments)]
      [(<) (apply < arguments)]
      [(>) (apply > arguments)]
      [(<=) (apply <= arguments)]
      [(>=) (apply >= arguments)]
      [(cons) (cons (1st arguments) (2nd arguments))]
      [(cdr) (cdr (1st arguments))]
      [(null?) (null? (1st arguments))]
      [(assq) (assq (1st arguments) (2nd arguments))]
      [(eq?) (eq? (1st arguments) (2nd arguments))]
      [(equal?) (equal? (1st arguments) (2nd arguments))]
      [(atom?) (lambda (x) (and (not (null? x)) (not (pair? x)))) (1st arguments)]
      [(length) (length (1st arguments))]
      [(list->vector) (list->vector (1st arguments))]
      [(list?) (list? (1st arguments))]
      [(pair?) (pair? (1st arguments))]
      [(procedure?) (closure? (1st arguments))]
      [(vector->list) (vector->list (1st arguments))]
      [(vector) (apply vector arguments)]
      [(make-vector) (make-vector (1st arguments) (2nd arguments))]      
      [(vector-ref) (vector-ref (1st arguments) (2nd arguments))]
      [(vector?) (vector? (1st arguments))]
      [(number?) (number? (1st arguments))]
      [(symbol?) (symbol? (1st arguments))]      
      [(vector-set!) (vector-set! (1st arguments) (2nd arguments) (3rd arguments))]
      [(display) (display (1st arguments))]
      [(newline) (newline)]
      [(caar) (caar (1st arguments))]
      [(cadr) (cadr (1st arguments))]
      [(cdar) (cdar (1st arguments))]
      [(cddr) (cddr (1st arguments))]
      [(caaar) (caaar (1st arguments))]
      [(caadr) (caadr (1st arguments))]
      [(cadar) (cadar (1st arguments))]
      [(caddr) (caddr (1st arguments))]
      [(cdaar) (cdaar (1st arguments))]
      [(cdadr) (cdadr (1st arguments))]
      [(cddar) (cddar (1st arguments))]
      [(cdddr) (cdddr (1st arguments))]
      [(map) (map (1st arguments) (2nd arguments))]
      [(apply) (apply (1st arguments) (2nd arguments))]
      [(assv) (assv (1st arguments) (2nd arguments))]
      [(append) (apply append arguments)]
      [else (error 'apply-prim-proc "Primitive procedure does not exist ~s" id)])))

(define *prim-proc-names* '(list car add1 - + * / sub1 zero? not = < > >= <= cons
                                 cdr null? assq eq? equal? atom? length list->vector
                                 list? pair? procedure? vector->list vector make-vector
                                 vector-ref vector? number? symbol? vector-set! display
                                 newline caar cadr cdar cddr caaar caadr cadar caddr cdaar
                                 cdadr cddar cdddr map apply assv append))

(define result->printable
  (lambda (result)
    (cond [(closure? result) '<interpreter-procedure>]
          [(list? result) (map result->printable result)]
          [else result])))

(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-exp exp)]
           ;[initial-environment global-env]
           [reduced-parse-tree (syntax-expand parse-tree)]
           ;[initial-environment (init-env)]
           ;[initial-environment (empty-env)]
           ;[result (eval-expression reduced-parse-tree initial-environment)])
           [result (eval-expression reduced-parse-tree (empty-env))])
      (result->printable result))))


;; Simply to pass the test cases
(define top-level-eval
  (lambda (exp)
    (eval-one-exp exp)))

;(trace eval-one-exp)
;(trace apply-prim-proc)
;(trace eval-expression)
;(trace parse-exp)
;(trace syntax-expand)
;(trace apply-env)
;(trace apply-proc)

(define find-position
  (lambda (sym ls pos)
    (cond [(null? ls) #f]
          [(eq? sym (car ls)) pos]
          [else (find-position sym (cdr ls) (+ pos 1))])))


(define global-env (make-hash))

(define add-to-global-env
  (lambda (id value)
    (hash-set! global-env id value)))

(define initialize-global-env
  (lambda ()
    (for-each (lambda (prim-name) (hash-set! global-env prim-name (primitive prim-name))) *prim-proc-names*)))

(initialize-global-env)

;(define global-env
;  (extend-env *prim-proc-names* (map primitive *prim-proc-names*) (empty-env)))



(define (rep)
  (begin
    (write '--> )
    (let ([input (read)])
      (write (eval-one-exp input))
      (newline)
      (rep))))
