#lang racket

(provide interval-contains? interval-intersects? interval-union make-vec-from-points dot-product vector-magnitude distance)

; 1
(define interval-contains?
  (lambda (a b)
    (if (<= (car a) b)
        (if (>= (car (cdr a)) b) #t #f)
        #f)))

;2
(define interval-intersects?
  (lambda (i1 i2)
    (if (or (interval-contains? i2 (car i1)) (interval-contains? i1 (car i2)))
        #t #f)))

;3
(define interval-union
  (lambda (i1 i2)
     (if (interval-intersects? i1 i2)
        (if (< (car i1) (car i2))
            (if (< (car (cdr i1)) (car (cdr i2)))
                (list (list (car i1) (car (cdr i2))))
                (list (list (car i1) (car (cdr i1)))))
            (if (< (car (cdr i1)) (car (cdr i2)))
                (list (list (car i2) (car (cdr i2))))
                (list (list (car i2) (car (cdr i1))))))
        (list i1 i2))))

;4
(define make-vec-from-points
  (lambda (p1 p2)
    (if (null? p1) null
        (cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2))))))

;5
(define dot-product
  (lambda (v1 v2)
    (if (null? v1) 0
        (+ (* (car v1) (car v2)) (dot-product (cdr v1) (cdr v2))))))

;6
(define vector-magnitude
  (lambda (v)
        (sqrt (dot-product v v))))

;7
(define distance
  (lambda (p1 p2)
    (vector-magnitude (make-vec-from-points p1 p2))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
