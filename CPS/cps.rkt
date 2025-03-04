;;;; Alex Anisimov & Naziia Raitova


#lang racket
(require "chez-init.rkt")
(require racket/trace)

(provide continuation halt-cont member?-cps set?-cps intersection-cps make-cps andmap-cps matrix?-cps)


(define anything?
  (lambda (_) #t))


(define-datatype continuation continuation?
  [halt-cont]
  [intersection-cont
   (e number?)
   (ls1 list?)
   (ls2 list?)
   (next-cont continuation?)]
  [set-cont
   (ls list?)
   (next-cont continuation?)]
  [add-cont
   (n number?)
   (next-cont continuation?)]
  [cons-cont
   (v anything?)
   (next-cont continuation?)]
  [andmap-cont
   (pred-cps procedure?)
   (ls list?)
   (next-cont continuation?)]
  [matrix-cont
   (ls list?)
   (next-cont continuation?)]
  [if-cont
   (iftrue procedure?)
   (iffalse procedure?)
   (next-cont continuation?)]
)


(trace-define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]
      [set-cont (ls next-cont)
                (if val
                    (apply-cont next-cont #f)
                    (set?-cps ls next-cont))]               
      [intersection-cont (e ls1 ls2 next-cont)
                         (if val
                             (intersection-cps ls1 ls2 (cons-cont e next-cont))
                             (intersection-cps ls1 ls2 next-cont))]
      [add-cont (n next-cont)
		     (apply-cont next-cont (+ val n))]
      [cons-cont (v next-cont)
                 (apply-cont next-cont (cons v val))]
      [andmap-cont (pred-cps ls next-cont)
                   (if val
                       (andmap-cps pred-cps ls next-cont)
                       (apply-cont next-cont #f))]
      [matrix-cont (ls next-cont)
                   (andmap-cps (make-cps (lambda (L)
                                             (length-cps L next-cont)
                                                (lambda (l)
                                                  (length-cps (car ls) next-cont))
                                           ))
                                 (cdr ls)
                                 next-cont)
                   ]
      [if-cont (iftrue iffalse next-cont)
               (if val
                   (iftrue next-cont)
                   (iffalse next-cont))]
      )))


;(andmap (lambda (L) (= (length L) (length (car m))))   
 ;                (cdr m)))))


(define member?-cps
  (lambda (e ls k)
    (if (null? ls)
        (apply-cont k #f)
        (if (eq? (car ls) e)
            (apply-cont k #t)
            (member?-cps e (cdr ls) k)))))

(define set?-cps
  (lambda (ls k)
    (if (null? ls)
        (apply-cont k #t)
        (member?-cps (car ls) (cdr ls) (set-cont (cdr ls) k)))))

(define intersection-cps
  (lambda (los1 los2 k)
    (if (null? los1)
        (apply-cont k '())
        (member?-cps (car los1) los2 (intersection-cont (car los1) (cdr los1) los2 k)))))

(define make-cps
  (lambda (proc)
    (lambda (args k)
      (apply-cont k (proc args)))))

(define andmap-cps
  (lambda (pred-cps ls k)
    (if (null? ls)
        (apply-cont k #t)
        (pred-cps (car ls) (andmap-cont pred-cps (cdr ls) k)))))


(define matrix?
  (lambda (m)
    (and (list? m)
         (not (null? m))   
         (not (null? (car m)))   
         (andmap list? m)
         (andmap (lambda (L) (= (length L) (length (car m))))   
                 (cdr m)))))

(trace-define matrix?-cps
  (lambda (m k)
    (apply-cont k 
                (if (list? m)
                    (if (not (null? m))
                        (if (not (null? (car m)))
                            (andmap-cps (make-cps list?) m (matrix-cont m k))
                            (apply-cont k #f))
                        (apply-cont k #f))
                    (apply-cont k #f))
                )))


(define length-cps
  (lambda (ls k)
    (cond [(null? ls) (apply-cont k 0)]
	  [else (length-cps (cdr ls) (add-cont 1 k))])))


(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))