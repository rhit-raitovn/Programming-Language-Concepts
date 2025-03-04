#lang racket

; Naziia Raitova - 12/5
(provide sum-of-squares range my-set? union more-positives? add-quotes get-304-quine)


;#1
(define sum-of-squares
  (lambda (a)
    (if (null? a)
        0
        ;else
        (+ (* (car a) (car a)) (sum-of-squares (cdr a)))
    )))

;#2
(define range
  (lambda (a b)
    (if (or (eq? a b) (> a b ))
        '()
        (append (list a) (range (+ a 1) b))
    )))

;#3
(define my-set?
  (lambda (a)
    (if (null? a)
        #t
        (if (member (car a) (cdr a))
            #f
            (my-set? (cdr a))))))

;#4
(define union
  (lambda (a b)
    (append a b)))

;#5
; sign is > for positive and < for negative
(define count-pos-neg
  (lambda (lon sign)
    (if (null? lon)
        0
        (if (sign (car lon) 0)
            (+ 1 (count-pos-neg (cdr lon) sign))
            (+ 0 (count-pos-neg (cdr lon) sign))))))

(define more-positives?
  (lambda (lon)
    (if (> (count-pos-neg lon >) (count-pos-neg lon <))
        #t #f)))

;#6
(define add-quotes
  (lambda (val num)
    (if (eq? 0 num)
        val
        (add-quotes (list 'quote val) (- num 1)))))
           
; Stuff for the final quine problem

(define get-304-quine
  (lambda ()
    "((lambda (f304) (list f304 (list (quote quote) f304))) (quote (lambda (f304) (list f304 (list (quote quote) f304)))))"))

(define eval-string
  (lambda (str)
    (let ((outp (open-output-string)))
      (parameterize ([current-output-port outp])
        (printf "~s" (eval (read (open-input-string str)) (make-base-namespace))))
      (get-output-string outp))))

(define is-quine-string?
 (lambda (str)
   (let ((result (eval-string str)))
     (if (equal? result str)
         #t
         (begin
           (printf "NOT QUINE~nIn : ~s~nOut: ~s" str result)
           #f)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
