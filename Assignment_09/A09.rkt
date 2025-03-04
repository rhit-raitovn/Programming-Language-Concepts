#lang racket

(provide sn-list-recur sn-list-sum sn-list-map sn-list-paren-count sn-list-reverse sn-list-occur sn-list-depth bt-recur bt-sum bt-inorder)

(define sn-list-recur
  (lambda (flist fatom init)
    (letrec ([helper (lambda (l)
                       (cond [(null? l) init]
                             [(or (pair? (car l))
                                  (null? (car l))) (flist (helper (car l))
                                                          (helper (cdr l)))]
                             [else (fatom (car l) (helper (cdr l)))]))])
      helper)))



(define sn-list-sum
  (lambda (snlist)
    ((sn-list-recur + + 0) snlist)))

(define sn-list-map
  (lambda (proc snlist)
    ((sn-list-recur
      (lambda (x y) (cons x y))
      (lambda (x y) (cons (proc x) y))
      '()) snlist)))

(define sn-list-paren-count
  (lambda (snlist)
    ((sn-list-recur (lambda (x y) (+ x y))
                    (lambda (x y) y)
                    2) snlist)))
                    

(define sn-list-reverse
  (lambda (snlist)
    ((sn-list-recur (lambda (x y) (append y (list x)))
                    (lambda (x y) (append y (list x)))
                    '())
     snlist)))

(define sn-list-occur
  (lambda (a snlist)
    ((sn-list-recur (lambda (x y) (+ x y))    ;flist
                    (lambda (x y) (if (equal? x a) (+ 1 y) y)) ;fatom
                    0)
     snlist)))

(define sn-list-depth
  (lambda (snlist)
    ((sn-list-recur (lambda (x y) (max (+ x 1) y))    ;flist
                    (lambda (x y) y)       ;fatom
                    1)
     snlist)))

; <bintree> ::= <number>  | (<symbol> <bintree> <bintree> )

(define bt-recur
  (lambda (root-proc number-proc init)
    (letrec ([helper (lambda (bt)
                       (cond [(null? bt) init]
                             [(number? bt) (number-proc bt)]
                             [else (root-proc (car bt) (helper (cadr bt))
                                                               (helper(caddr bt)))]))])
      helper)))

(define bt-sum
  (lambda (bt)
    ((bt-recur (lambda (x y z) (+ y z))  ; root proc
                    (lambda (x) x) ; number proc
                    0)
     bt)))

(define bt-inorder
  (lambda (bt)
    ((bt-recur (lambda (x y z) (append y (list x) z))   ; root proc
                    (lambda (x) '()) ; number proc
                    '())
     bt)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
