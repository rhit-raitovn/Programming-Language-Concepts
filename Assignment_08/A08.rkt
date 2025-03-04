#lang racket
(require racket/trace)
(require "chez-init.rkt")

(provide make-slist-leaf-iterator subst-leftmost)

(define make-stack
    (lambda ()
        (let ([stk '()])
            (lambda (msg  . args ) 
                (case msg   ; Scheme's case is a similar to switch in some other languages.
                    [(empty?) (null? stk)]
                    [(push)   (set! stk (cons (car args) stk))]
                    [(pop)    (let ([top (car stk)])
                            (set! stk (cdr stk))
                            top)]
                    [else (error "illegal message to stack object:" msg)])))))

(define make-slist-leaf-iterator
  (lambda (slist)
    (let ([stack (make-stack)])
      (for-each (lambda (x) (stack 'push x)) (reverse (flatten slist)))
      (letrec ([my-iter
                (lambda (msg)
                  (case msg
                    [(next) (if (stack 'empty?)
                                #f
                                (stack 'pop))]
                    [else (error "illegal message to slist-leaf-iterator object:" msg)]))])
        my-iter))))
;;(trace make-slist-leaf-iterator)
                        
(define subst-leftmost
  (lambda (new old slist proc?)
    (car (subst-helper new old slist proc? #f))))

;; got help from ta
(define subst-helper
  (lambda (new old slist proc? changed)
    (cond
      [changed (list slist #t)]
      [(null? slist) (list slist #f)]
      [(list? (car slist))
       (let ([first-change (subst-helper new old (car slist) proc? changed)])
         (if (and (pair? first-change) (not (cadr first-change)))
             (let ([next-change (subst-helper new old (cdr slist) proc? (cadr first-change))])
               (if (and (pair? next-change))
                   (list (cons (car first-change) (car next-change))
                         (cadr next-change))
                   (list (car slist) #f)))
             (list (cons (car first-change) (cdr slist))
                   #t)))]  
      [else (if (proc? (car slist) old)
                (let ([temp (subst-helper new old (cdr slist) proc? #t)])
                  (if (and (pair? temp))
                      (list (cons new (car temp))
                            #t)
                      (list (list new) #t)))  
                (let ([temp (subst-helper new old (cdr slist) proc? #f)])
                  (if (and (pair? temp))
                      (list (cons (car slist) (car temp))
                            (cadr temp))
                      (list (list (car slist)) #f))))])))


                             
;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))

;; helpers
(define (flatten lst)
  (letrec ([helper
             (lambda (l)
               (cond
                 [(null? l)
                  '()]
                 [(null? (car l))
                  (helper (cdr l))]
                 [(pair? (car l))
                  (append (helper (car l))
                          (helper (cdr l)))]
                 [else
                  (append (list (car l))
                          (helper (cdr l)))]))])
    (helper lst)))
