#lang racket

(provide intersection subset? relation? domain reflexive? multi-set? ms-size last all-but-last)

(define intersection
  (lambda (a b)
    (if (null? a)
        '()
        (if (member (car a) b)
            (append (list (car a)) (intersection (cdr a) b))
            (intersection (cdr a) b)))))

; added list? to my-set? from A02
; TA said I'm allowed to use member
(define set?
  (lambda (a)
    (if (list? a)
        (if (null? a)
            #t
            (if (member (car a) (cdr a))
                #f
                (set? (cdr a))))
        #f)))

(define relation?
  (lambda (a)
    (if (set? a)
        (if (null? a)
            #t
            (if (list? (car a))
                (if (= (length (car a)) 2)
                    (and #t (relation? (cdr a)))
                    #f)
                #f))
        #f)))

(define domain
  (lambda (a)
    (domain-helper a '())))

(define domain-helper
  (lambda (a dom)
    (if (null? a) dom
    (if (member (car (car a)) dom)
        (domain-helper (cdr a) dom)
        (append dom (list (car (car a))) (domain-helper (cdr a) dom))))))

(define reflexive?
  (lambda (a)
    (reflexive-helper? a a)))

(define reflexive-helper?
  (lambda (a b)
    (if (null? a) #t
        (if (relation? a)
            (if (and (member (list (car (car a)) (car(car a))) b)
                     (member (list (cadr (car a)) (cadr(car a))) b))
                (reflexive-helper? (cdr a) b)
                #f)
            #f)
    )))

(define multi-set?
  (lambda (a)
    (multi-set-helper? a '())))

(define multi-set-helper?
  (lambda (a combined)
    (if (null? a) #t
    (if (and (list? a)(list? (car a)))
        (if (or (not (integer? (cadr (car a)))) (not (symbol? (car (car a)))))
            #f 
            (if (not (positive? (cadr (car a))))
                #f
                (if (member (car (car a)) combined)
                     #f
                     (multi-set-helper? (cdr a) (append combined (list (car (car a))))))))
        #f))))

(define ms-size
  (lambda (a)
    (if (null? a) 0
    (if (not (multi-set? a)) #f
        (+ (cadr (car a)) (ms-size (cdr a)))))))

(define last
  (lambda (a)
    (if (= (length a) 1) (car a)
    (last(cdr a)))))

(define all-but-last
  (lambda (a)
    (if (= (length a) 1) '()
    (append (list(car a)) (all-but-last(cdr a))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
