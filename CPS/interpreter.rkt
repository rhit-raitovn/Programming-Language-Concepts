;;; ALEX AND NAZIIA
; CPS II


#lang racket
(require "chez-init.rkt")
(require racket/trace)
(provide eval-one-exp top-level-eval)

(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)])
      (cases expression parse-tree
        [break-exp (vals)
                   (eval-break-values-cps vals (empty-env) (halt-cont))]
        [else 
          (let ([result (eval-expression parse-tree (halt-cont) (empty-env))])
            (result->printable result))]))))

(define top-level-eval
  (lambda (exp)
    (eval-one-exp exp)))

(define result->printable
  (lambda (result)
    (cond [(proc-val? result) '<interpreter-procedure>]
          [(list? result) (map result->printable result)] ; map ok
          [(eqv? (void) result) '(void)]
          [else result])))


(define eval-expression
  (lambda (exp cont env)
    (cases expression exp
      [lit-exp (val) (apply-cont cont val)]
      [var-exp (id) (apply-cont cont (apply-env env id))]
      [if-exp (test-exp iftrue-exp iffalse-exp)
              (eval-expression test-exp (if-cont iftrue-exp iffalse-exp cont env) env)]
      [app-exp (proc-and-args)
               (eval-expressions-cps proc-and-args (proc-cont cont) env)]
      ; LAMBDA
      [lambda-exp (ids body) (apply-cont (lambda-cont ids body env cont) #f)]
      
      [while-exp (condition exp env)
           (eval-expression condition
                            (while-condition-cont condition exp env cont)
                            env)]


      [set-exp (id body) (eval-expression body
                                          (set-cont env id cont)
                                          env)]
      [define-exp (id value)
               (eval-expression value
                                (define-env-cont id cont)
                                env)]
      [let-exp (syms exps bodies)
               (eval-expression (app-exp (cons (lambda-exp syms bodies) exps))
                                cont
                                env)]

      [letrec-exp (ids bodies)
                  ; MAP TODO
                  (let* [(syms (map 1st ids))
                         (vals (map 2nd ids))
                         (new-env (extend-env syms 
                                            (make-list (length syms) #f)
                                            env))]
                    (eval-letrec-bindings syms vals bodies new-env cont))]

      [begin-exp (bodies)
                 (eval-begin-expressions-cps bodies env cont)]

      [break-exp (vals)
                 (eval-break-values-cps vals env (halt-cont))]
                
      [else (error 'eval-expression "not implemented ~s" exp)])
    ))
;(trace eval-expression)

(define eval-break-values-cps
  (lambda (vals env cont)
    (if (null? vals)
        (apply-cont cont '())
        (eval-break-exp-cps (car vals) env
                           (break-values-cont (cdr vals) env cont)))))

(define eval-break-exp-cps
  (lambda (exp env cont)
    (cases expression exp
      [lit-exp (val) (apply-cont cont val)]
      [var-exp (id) (apply-cont cont (apply-env env id))]
      [break-exp (vals) (eval-break-values-cps vals env (halt-cont))]
      [else (eval-expression exp cont env)])))

(define eval-letrec-bindings
  (lambda (syms vals bodies env cont)
    (if (null? syms)
        (eval-letrec-bodies bodies env cont)
        (eval-expression (car vals)
                        (letrec-bind-cont (cdr syms)
                                        (cdr vals)
                                        bodies
                                        (car syms)
                                        env
                                        cont)
                        env))))

(define eval-letrec-bodies
  (lambda (bodies env cont)
    (if (null? (cdr bodies))
        (eval-expression (car bodies) cont env)
        (eval-expression (car bodies)
                        (letrec-body-cont (cdr bodies) env cont)
                        env))))


(define eval-expressions-cps
  (lambda (proc-and-args cont env)
    (if (null? proc-and-args)
        (apply-cont cont '())
        (eval-expression (car proc-and-args)
                         (eval-exps-cont
                          cont
                          (cdr proc-and-args)
                          ;rator
                          ;(cdr rands)
                          env)
                         env))))

;(trace eval-expressions-cps)

;;; Continuations

(define anything?
  (lambda (_) #t))

(define-datatype continuation continuation?
  [halt-cont]
  [if-cont
   (true-exp expression?)
   (false-exp expression?)
   (cont continuation?)
   (env list?)]
  [eval-exps-cont
   (cont continuation?)
   (proc-and-args (listof expression?))
   (env list?)]
  [cons-cont
   (k continuation?)
   (val anything?)]
  [proc-cont
   ;(rands list?)
   (cont continuation?)]
  [map-head-k
   (proc-cps procedure?)
   (ls list?)
   (k continuation?)]
  [map-tail-k
   (head anything?)
   (k continuation?)]

  [while-condition-cont
   (condition expression?)
   (exp (listof expression?))
   (env environment?)
   (k continuation?)]

  [while-body-cont
   (condition expression?)
   (exp (listof expression?))
   (env environment?)
   (k continuation?)]
  
  [while-cont
   (condition expression?)
   (exp (listof expression?))
   (env environment?)
   (k continuation?)]
  
  [eval-bodies-cont
   (bodies list?)
   (env environment?)
   (k continuation?)]
  [begin-cont
   (remaining-exps (listof expression?))
   (env environment?)
   (cont continuation?)]  

  ;NEW
  [set-cont
   (env environment?)
   (id symbol?)
   (cont continuation?)]
  [define-env-cont
    (id symbol?)
    (cont continuation?)]

  ; LET
  [letrec-cont
   (sym symbol?)
   (val expression?)
   (env environment?)
   (cont continuation?)]
  [letrec-bind-cont
   (remaining-syms (listof symbol?))
   (remaining-vals (listof expression?))
   (bodies (listof expression?))
   (current-sym symbol?)
   (env environment?)
   (cont continuation?)]
  [letrec-body-cont
   (remaining-bodies (listof expression?))
   (env environment?)
   (cont continuation?)]

  ; LAMBDA
  [lambda-cont
   (ids (lambda (x) (or (symbol? x) (nullablePair? x))))
   (bodies pair?)
   (env environment?)
   (cont continuation?)]
  [break-values-cont
   (remaining-vals (listof expression?))
   (env environment?)
   (cont continuation?)]
        
  
  [exit-cont ]
  )

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]
      [if-cont (if-true-exp if-false-exp next-cont env)
               (if val
                   (eval-expression if-true-exp next-cont env)
                   (eval-expression if-false-exp next-cont env))]
      [eval-exps-cont (k exps env)
                      (eval-expressions-cps exps (cons-cont k val) env)]
      [cons-cont (k v)
                 (apply-cont k (cons v val))]
      [proc-cont (cont)
                 (apply-proc-cps (car val) (cdr val) cont)]
      
      [map-head-k (proc-cps ls k)
         (map-cps proc-cps (cdr ls) (map-tail-k val k))]
      [map-tail-k (head k)
         (apply-cont k (cons head val))]

      [while-condition-cont (condition exp env cont)
                            (if val
                                (eval-bodies exp env (while-body-cont condition exp env cont))
                                (apply-cont cont (void)))]

      [while-body-cont (condition exp env cont)
                       (eval-expression (while-exp condition exp env)
                                        cont
                                        env)]

      [while-cont (condition exp env cont)
                  (eval-expression (while-exp condition exp env)
                                   cont
                                   env)]

      [eval-bodies-cont (bodies env k)
                     (eval-bodies (cdr bodies) env k)]
      [begin-cont (remaining-exps env cont)
                  (eval-begin-expressions-cps remaining-exps env cont)]
      [break-values-cont (remaining-vals env cont)
                        (if (null? remaining-vals)
                            (apply-cont cont (list val))
                            (eval-break-exp-cps (car remaining-vals) 
                                              env
                                              (break-values-cont 
                                               (cdr remaining-vals)
                                               env
                                               (cons-cont cont val))))]

      ;(set-cont env id cont)
      [set-cont (env id cont)
        (apply-cont cont (redefine-variable env id val))]

      [define-env-cont (id cont)
        (apply-cont cont (add-to-global-env id val))]

      ; LET
      [letrec-cont (sym val new-env cont)
                   (apply-cont cont (redefine-variable new-env sym val))]
      [letrec-bind-cont (remaining-syms remaining-vals bodies current-sym env cont)
                        (begin
                          (redefine-variable env current-sym val)
                          (eval-letrec-bindings remaining-syms
                                               remaining-vals
                                               bodies
                                               env
                                               cont))]
      [letrec-body-cont (remaining-bodies env cont)
                        (eval-letrec-bodies remaining-bodies env cont)]
      
      ; LAMBDA
      ; (lambda-cont ids body env cont)
      [lambda-cont (ids body env cont)
                   (apply-cont cont (closure-record ids body env))]

      [exit-cont () (display "Exiting interpreter.\n") (void)]

      )))
;(trace apply-cont)

(define add-to-global-env
  (lambda (id value)
    (hash-set! global-env id value)))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (listof symbol?))   
   (vals vector?)
   (env environment?)])


;(define redefine-variable
;  (lambda (env sym value)
;    (if (null? env)
;        (if (hash-has-key? global-env sym)
;                            (hash-set! global-env sym value)
;                            (error 'redefine-variable "Temporary error - can't set before defining (for ~s)" sym))
;        (let ([pos (find-position sym symbols 0)])
;                             (if (number? pos)
;                                 (vector-set! values pos value)
;                                 (redefine-variable env sym value))))))

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
;(trace redefine-variable)

(define eval-bodies
    (lambda (bodies env k)
        (if (null? (cdr bodies))
            (eval-expression (car bodies) k env)
            (eval-expression (car bodies)
                             (eval-bodies-cont bodies env k)
                             env))))

; (define map-cps(lambda (proc-cps ls k)
(define apply-proc-cps
  (lambda (proc args cont)
    (cases proc-val proc
      [prim-proc (id) (apply-prim-proc (prim-proc id) args cont)]
      [closure-record (ids body env)
                      (let ([new-env (extend-env (if (symbol? ids) (list ids) ids) 
                                                 args 
                                                 env)])
                        (eval-bodies body new-env cont))]
      [continuation-proc (k)
                    (apply-cont k (car args))]
      
      [exit-proc () (apply-cont (exit-cont) args)]
      [else (error 'apply-proc-cps "Expected a function, but got ~s" proc)])))

;;(lambda (b) (eval-expression b new-env))
;(trace apply-proc-cps)

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)


(define apply-prim-proc
  (lambda (proc arguments cont)
    (cases proc-val proc
      [prim-proc (id)
                 (case id
                   [(list) (apply-cont cont arguments)]
                   [(car) (apply-cont cont (caar arguments))]
                   [(add1) (apply-cont cont (+ (car arguments) 1))]
                   [(-) (apply-cont cont (apply - arguments))]
                   [(+) (apply-cont cont (apply + arguments))]
                   [(*) (apply-cont cont (apply * arguments))]
                   [(/) (apply-cont cont (apply / arguments))]
                   [(sub1) (apply-cont cont (- (1st arguments) 1))]
                   [(zero?) (apply-cont cont (zero? (1st arguments)))]
                   [(not) (apply-cont cont (not (1st arguments)))]
                   [(=) (apply-cont cont (apply = arguments))]
                   [(<) (apply-cont cont (apply < arguments))]
                   [(>) (apply-cont cont (apply > arguments))]
                   [(<=) (apply-cont cont (apply <= arguments))]
                   [(>=) (apply-cont cont (apply >= arguments))]
                   [(cons) (apply-cont cont (cons (1st arguments) (2nd arguments)))]
                   [(cdr) (apply-cont cont (cdr (1st arguments)))]
                   [(null?) (apply-cont cont (null? (1st arguments)))]
                   [(assq) (apply-cont cont (assq (1st arguments) (2nd arguments)))]
                   [(eq?) (apply-cont cont (eq? (1st arguments) (2nd arguments)))]
                   [(equal?) (apply-cont cont (equal? (1st arguments) (2nd arguments)))]
                   [(atom?) (apply-cont cont (lambda (x) (and (not (null? x)) (not (pair? x)))) (1st arguments))]
                   [(length) (apply-cont cont (length (1st arguments)))]
                   [(list->vector) (apply-cont cont (list->vector (1st arguments)))]
                   [(list?) (apply-cont cont (list? (1st arguments)))]
                   [(pair?) (apply-cont cont (pair? (1st arguments)))]
                   ;[(procedure?) (closure? (1st arguments)))]
                   [(procedure?) (apply-cont cont (proc-val? (1st arguments)))]
                   [(vector->list) (apply-cont cont (vector->list (1st arguments)))]
                   [(vector) (apply-cont cont (apply vector arguments))]
                   [(make-vector) (apply-cont cont (make-vector (1st arguments) (2nd arguments)))]      
                   [(vector-ref) (apply-cont cont (vector-ref (1st arguments) (2nd arguments)))]
                   [(vector?) (apply-cont cont (vector? (1st arguments)))]
                   [(number?) (apply-cont cont (number? (1st arguments)))]
                   [(symbol?) (apply-cont cont (symbol? (1st arguments)))]      
                   [(vector-set!) (apply-cont cont (vector-set! (1st arguments) (2nd arguments) (3rd arguments)))]
                   [(caar) (apply-cont cont (caar (1st arguments)))]
                   [(cadr) (apply-cont cont (cadr (1st arguments)))]
                   [(cdar) (apply-cont cont (cdar (1st arguments)))]
                   [(cddr) (apply-cont cont (cddr (1st arguments)))]
                   [(caaar) (apply-cont cont (caaar (1st arguments)))]
                   [(caadr) (apply-cont cont (caadr (1st arguments)))]
                   [(cadar) (apply-cont cont (cadar (1st arguments)))]
                   [(caddr) (apply-cont cont (caddr (1st arguments)))]
                   [(cdaar) (apply-cont cont (cdaar (1st arguments)))]
                   [(cdadr) (apply-cont cont (cdadr (1st arguments)))]
                   [(cddar) (apply-cont cont (cddar (1st arguments)))]
                   [(cdddr) (apply-cont cont (cdddr (1st arguments)))]
                   [(map) (map-cps (lambda (arg k)
                                (apply-proc-cps (1st arguments) (list arg) k))
                            (2nd arguments) cont)]
                   ;[(apply) (apply-cont cont (apply (1st arguments) (2nd arguments)))]
                   [(apply) (apply-proc-cps (1st arguments) (2nd arguments) cont)]
                   [(assv) (apply-cont cont (assv (1st arguments) (2nd arguments)))]
                   [(append) (apply-cont cont (apply append arguments))]
                   [(exit) (apply-proc-cps (exit-proc) arguments cont)]
                   [(call/cc) (apply-proc-cps (car arguments)
                                   (list (continuation-proc cont))
                                   cont)]
                   
                   [else (error 'apply-prim-proc "Primitive procedure does not exist ~s" id)])]
      [closure-record (ids body env)
                     (error 'apply-prim-proc "Closure record should not be handled by apply-prim-proc")]
      [continuation-proc (k)
                     (error 'apply-prim-proc "should not be handled by apply-prim-proc")]
      [exit-proc () (apply-proc-cps (exit-proc) arguments cont)])))

(define map-cps
  (lambda (proc-cps ls k)
    (if (null? ls)
        (apply-cont k '())
        (proc-cps (car ls) (map-head-k proc-cps ls k)))))

(define-datatype expression expression?
  [lit-exp
   (value anything?)]
  [var-exp
   (id symbol?)]
  [if-exp
   (test-exp expression?)
   (true-exp expression?)
   (false-exp expression?)]
  [app-exp
   (proc-and-args (listof expression?))]

  ;NEW
  [while-exp
   (condition expression?)
   (exp (listof expression?))
   (env environment?)]
  [define-exp
    (id symbol?)
    (val expression?)]
  [set-exp
   (id symbol?)
   (body expression?)]
  [let-exp
   (syms (listof symbol?))
   (exps (listof expression?))
   (bodies (listof expression?))]

  ;LET
  [letrec-exp
   (ids nullablePair?)
   (bodies pair?)]

  ;lambda
  [lambda-exp
   (ids (lambda (x) (or (symbol? x) (nullablePair? x))))
   (bodies pair?)]
  [begin-exp
   (bodies (listof expression?))]

  [break-exp
   (vals (listof expression?))]

  )

(define nullablePair?
  (lambda (lst)
    (or (pair? lst) (null? lst))))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
          [(number? datum) (lit-exp datum)]
          [(boolean? datum) (lit-exp datum)]
          [(string? datum) (lit-exp datum)]
          [(pair? datum)
           (cond [(eq? (car datum) 'quote) (lit-exp (cadr datum))]
                 [(eqv? (car datum) 'if)
                  (if-exp (parse-expression (cadr datum))
                          (parse-expression (caddr datum))
                          (parse-expression (cadddr datum)))]
                 [(eqv? (car datum) 'lambda)
                  (if (or (null? (cddr datum)) (not (valid-lambda-args? (2nd datum))))
                      (error 'parse-exp "bad expression: ~s" datum)
                      (lambda-exp
                       (2nd datum)
                       (map ; map ok
                        parse-expression
                        (cddr datum))))]
                 [(eqv? (car datum) 'quote)
                  (lit-exp (cadr datum))]
                 [(eq? (car datum) 'while)
                  (while-exp (parse-expression (cadr datum))
                             (map parse-expression (cddr datum)) ; map ok
                             (empty-env))]
                 [(eqv? (1st datum) 'define)
                  (define-exp (2nd datum) (parse-expression (3rd datum)))]
                 [(eqv? (1st datum) 'set!)
                  (set-exp (2nd datum) (parse-expression (3rd datum)))]
                 [(eqv? (car datum) 'letrec)
                  (letrec-exp (map
                               (lambda (id)
                                 (list (car id) (parse-expression (cadr id))))
                               (cadr datum))
                              (map
                               parse-expression
                               (cddr datum)))]
                 [(equal? (1st datum) 'let)
                  (let-exp (map car (2nd datum)) (map parse-expression (map cadr (2nd datum))) (map parse-expression (cddr datum)))]

                 [(eqv? (car datum) 'begin)
                  (begin-exp (map parse-expression (cdr datum)))]
                 [(eqv? (car datum) 'break)
                  (break-exp (map parse-expression (cdr datum)))]
                 
                 [else
                  (app-exp(map (lambda (e) (parse-expression e)) datum))])]
          [else (error 'parse-expression
                       "Invalid concrete syntax ~s" datum)])))


; parse funcs
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

(define proper-list?
  (lambda (lst)
    (if (null? lst)
        #t
        (and (pair? lst) (proper-list? (cdr lst))))))

;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define eval-begin-expressions-cps
  (lambda (exps env cont)
    (if (null? (cdr exps))
        (eval-expression (car exps) cont env)
        (eval-expression (car exps)
                        (begin-cont (cdr exps) env cont)
                        env))))

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

;(trace apply-env)

(define find-position
  (lambda (sym ls index)
    (cond [(null? ls) #f]
          [(eq? sym (car ls)) index]
          [else (find-position sym (cdr ls) (+ index 1))])))

;(trace find-position)

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure-record
   (id (listof symbol?))
   (body nullablePair?)
   (env environment?)]
  [continuation-proc
   (cont continuation?)]
  [exit-proc ])


(define *prim-proc-names* '(list car add1 - + * / sub1 zero? not = < > >= <= cons
                                 cdr null? assq eq? equal? atom? length list->vector
                                 list? pair? procedure? vector->list vector make-vector
                                 vector-ref vector? number? symbol? vector-set! display
                                 newline caar cadr cdar cddr caaar caadr cadar caddr cdaar
                                 cdadr cddar cdddr map apply assv append call/cc exit))




(define global-env (make-hash))

(define initialize-global-env
  (lambda ()
    (for-each (lambda (prim-name)
                (hash-set! global-env prim-name (prim-proc prim-name)))
              *prim-proc-names*)))

(initialize-global-env)

(define rep
  (lambda ()
    (display "--> ")
    (let ([input (read)]) 
      (let ([result (top-level-eval (parse-expression input))])
        (if (eq? result (void))
            (void)
            (begin (pretty-print result)
                   (rep))
            )))))


;(trace extend-env)

;(eval-one-exp '(+ 3 5))
;(eval-one-exp '(while #f (display "Should not print")))
; (eval-one-exp '(while #t (+ 3 5) (while #f (+ 1 1))))
	 

