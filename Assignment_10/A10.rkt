#lang racket 

(provide free-vars bound-vars lexical-address un-lexical-address convert-multip-calls convert-multip-lambdas convert-ifs)
(require racket/trace)
(require "chez-init.rkt")


;<LcExpr> ::= <identifier> |
;             (lambda (<identifier>) <LcExpr>) |
;             ( <LcExpr> <LcExpr> )

(define occurs-free?
  (lambda (var exp)
    (cond [(symbol? exp) (eqv? var exp)]
	  [(eqv? (car exp) 'lambda)
	   (and (not (eqv? (caadr exp) var))
		(occurs-free? var (caddr exp)))]
	  [else (or (occurs-free? var (car exp))
		    (occurs-free? var (cadr exp)))])))

(define free-vars
  (lambda (exp)
    (free-vars-helper exp '())))

(define free-vars-helper
  (lambda (exp res)
    (cond [(null? exp) res]
          [(symbol? exp) (cons exp res)]
	  [(equal? (car exp) 'lambda)
           ;(    (lambda (x) (x y))    (z (lambda (y) (z y)))    )
           ; lambda + lambda exp
           (if (list? (caddr exp))
               (let ([next (free-vars-helper (caddr exp) '())])
                 (append (filter (lambda (x) (not (equal? (caadr exp) x))) next) res))
               (if (not (member (caddr exp) (cadr exp)))
                   (cons (caddr exp) res)
                   res))]

                 
	  [else (let ([first-exp (free-vars-helper (car exp) '())]
                      [second-exp (free-vars-helper (cadr exp) '())])
                  (if (equal? first-exp second-exp)
                      (append first-exp res)
                      (append first-exp second-exp res)))])))
                      
                     

(define bound-vars
  (lambda (exp)
    (bound-vars-helper exp '())))

(define bound-vars-helper
  (lambda (exp res)
    (cond [(null? exp) res]
          [(symbol? exp) res]
	  [(equal? (car exp) 'lambda)
           ; lambda + lambda exp
           (if (list? (caddr exp))
               (let ([val (caadr exp)]
                     [next (bound-vars-helper (caddr exp) '())])
                 (if (member val next)
                     (append next res)
                     (append (list val) next res)))
               (if (not (member (caddr exp) (cadr exp)))
                   res
                   (cons (caddr exp) res)))]  
	  [else (let ([first-exp (bound-vars-helper (car exp) '())]
                      [second-exp (bound-vars-helper (cadr exp) '())])
                  (if (equal? first-exp second-exp)
                      (append first-exp res)
                      (append first-exp second-exp res)))])))


;<SchemeliteExpr> ::= <identifier> |
;             (lambda (<identifier>+) <SchemeliteExp>) |
;             ( <SchemeliteExp> {<SchemeliteExp>}+ ) |
;             #f | #t |
;             (if <SchemeliteExp> <SchemeliteExp> <SchemeliteExp>) |
;             (let ({(<identifier <SchemeliteExp>)}+) <SchemeliteExp>)

(define convert-multip-calls
  (lambda (lcexp)
    (convert-helper lcexp '())))

(define convert-helper
  (lambda (exp res)
    (cond
      [(symbol? exp) exp]
      [(eqv? 'lambda (car exp))
       (list 'lambda (cadr exp) (convert-helper (caddr exp) (cons (cadr exp) res)))]
      [(eqv? 'if (car exp))
       (list 'if
             (convert-helper (cadr exp) res)
             (convert-helper (caddr exp) res)
             (convert-helper (cadddr exp) res))]
      [(eqv? 'let (car exp))
       (list 'let
             (map (lambda (x) (list (car x) (convert-helper (cadr x) res))) (cadr exp))
             (convert-helper (caddr exp) (cons (map car (cadr exp)) res)))]
      [else
       (if (and (list? exp) (not (null? exp)))
           (foldl (lambda (x acc) (if (null? acc) x (list acc x)))
                  '()
                  (map (lambda (x) (convert-helper x res)) exp))
           exp)])))
;(trace convert-helper)

(define convert-multip-lambdas
  (lambda (lcexp)
    (cond
      [(symbol? lcexp) lcexp]
      [(and (pair? lcexp) (eqv? 'lambda (car lcexp)))
       (let* ((params (cadr lcexp))
              (body (caddr lcexp)))
         ;; more than one parameter
         (if (pair? params)
             (foldr (lambda (param acc) (list 'lambda (list param) acc))
                    (convert-multip-lambdas body)
                    params)
             (list 'lambda params (convert-multip-lambdas body))))]
      [(list? lcexp)
       (map convert-multip-lambdas lcexp)]
      [else lcexp])))

(define convert-ifs-helper
  (lambda (boolean)
    (if boolean
        '(lambda (thenval elseval) thenval)
        '(lambda (thenval elseval) elseval))))

(define convert-ifs
  (lambda (lcexp)
    (cond
      [(null? lcexp) '()]
      [(boolean? lcexp) (convert-ifs-helper lcexp)]
      [(and (pair? lcexp) (eqv? (car lcexp) 'if))
       (let* ((cond (convert-ifs (cadr lcexp)))
              (then-branch (convert-ifs (caddr lcexp)))
              (else-branch (convert-ifs (cadddr lcexp))))
         (list cond then-branch else-branch))]
      [(and (pair? lcexp) (eqv? (car lcexp) 'lambda))
       (list 'lambda
             (cadr lcexp)
             (convert-ifs (caddr lcexp)))]
      [(list? lcexp)
       (map convert-ifs lcexp)]
      [else lcexp])))

;(trace convert-ifs)

(define lexical-address
  (lambda (exp)
    (letrec ([helper (lambda (exp res)
                       (cond[(symbol? exp) (builder exp res 0)]
                            [(eqv? 'lambda (car exp))
                             (list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) res)))]
                            [(eqv? 'if (car exp))
                             (cons 'if (helper (cdr exp) res))]
                            [(eqv? 'let (car exp))
                             (list 'let
                                   (map (lambda (x) (list (car x) (helper (cadr x) res))) (cadr exp))
                                   (helper (caddr exp) (cons (map car (cadr exp)) res))
                                   )]
                            [else
                             (map (lambda (x) (helper x res)) exp)]
                            ))])
      (helper exp '()))))

(define builder
  (lambda (sym exp depth)
    (if (null? exp)
        (list ': 'free sym)
        (let ([index (index-of sym (car exp))])
          (if (= -1 index)
              (builder sym (cdr exp) (+ depth 1))
              (list ': depth index))))))

(define index-of
  (lambda (item list)
    (if (null? list) -1
        (if (equal? (car list) item) 0
            (let ([rest (index-of item (cdr list))])
              (if (= rest -1)
                  -1
                  (+ 1 rest)))))))

(define un-lexical-address
  (letrec ([helper (lambda (exp res)
                     (cond[(= (length exp) 1)
                           (list (helper (car exp) res))]
                          [(eqv? (cadr exp) 'free)
                           (caddr exp)]
                          [(number? (cadr exp))
                           (list-ref (list-ref res (cadr exp)) (caddr exp))]
                          [(eqv? (car exp) 'lambda)
                           (list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) res)))]
                          [(eqv? (car exp) 'if)
                           (cons 'if (cons (helper (cadr exp) res) (helper (cddr exp) res)))]
                          [(eqv? (car exp) 'let)
                           (list 'let (map (lambda (x) (list (car x) (helper (cadr x) res))) (cadr exp))
                                 (helper (caddr exp) (cons (map car (cadr exp)) res)))]
                          [else
                           (map (lambda (x) (helper x res)) exp)]))])
    (lambda (exp)
      (helper exp '()))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
