#lang racket

(provide minimize-interval-list exists? product best remove-last)

; this first one is probably the hardest in the set
; so if you get stuck I'd try the later ones

;go thelp from a TA
(define minimize-interval-list
  (lambda (lst)
    (let ([sorted (sort lst (lambda (x y) (< (car x) (car y))))])
      (let loop ([lst sorted]
                 [result '()])
        (if (null? lst)
            (reverse result)
            (let ([current (car lst)])
              (if (null? (cdr lst)) ; if one element in the list
                  (loop '() (cons current result)) 
                  (let ([next (cadr lst)])
                    (if (< (cadr current) (car next)) ; no overlap
                        (loop (cdr lst) (cons current result))
                        (loop (cons (list (car current) (max (cadr current) (cadr next))) (cddr lst)) result))))))))))

(define exists?
  (lambda (a b)
    (number-helper? a b)))

(define number-helper?
  (lambda (proc lst)
    (if (null? lst) #f
        (if (not (null? (filter proc lst)))
            #t
            #f))))

(define running-sum
  (lambda (lst)
    (let loop ([lst lst]
               [sum 0])
      (if (null? lst) lst
          (let ([new-sum (+ sum (car lst))])                         ; 1 + 10 = 11
            (append (list new-sum) (loop (cdr lst) new-sum)))))))

(define best
  (lambda (proc lst)
    (let loop ([lst lst]
               [res (car lst)])
      (if (null? lst) res
      (let ([new-value (car lst)])
        (if (> (proc new-value) (proc res))
            (loop (cdr lst) new-value)
            (loop (cdr lst) res)))))))

(define product
  (lambda (a b)
    (cartesian-product a b)))

(define remove-last
  (lambda (a lst)
    (if (null? lst) lst
        (reverse (remove a (reverse lst))))))
          

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
