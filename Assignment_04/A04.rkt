#lang racket

(provide pop-song? running-sum invert combine-consec)

; got some help from a TA
(define pop-song?
  (lambda (lst)
    (letrec
        ((state-1 ; the starting state
          (lambda (lst)
            (if (null? lst)
                #f
                ( if (eq? (car lst) 'verse)
                     (state-2 (cdr lst))
                     #f))))

         (state-2 ; the state we're in when we've just finished a verse
          (lambda (lst)
            (if (null? lst)
                #f
                (if (eq? (car lst) 'refrain)
                    (state-3 (cdr lst))
                    ( if (eq? (car lst) 'guitar-solo)
                         (state-2 (cdr lst))
                         #f)))))

         (state-3 ; the state we're in when we've just finished a refrain
          (lambda (lst)
            ( cond
               ((null? lst) #f)
               ((eq? (car lst) 'refrain)
                (if (null? (cdr lst))
                    #t #f))
               ((eq? (car lst) 'verse) (state-2 (cdr lst)))
               ((eq? (car lst) 'guitar-solo) (state-3 (cdr lst)))
               (else #f))))) 

      (state-1 lst))))


(define running-sum
  (lambda (lst)
    (let loop ([lst lst]
               [sum 0])
      (if (null? lst) lst
          (let ([new-sum (+ sum (car lst))])                         ; 1 + 10 = 11
            (append (list new-sum) (loop (cdr lst) new-sum)))))))
           
(define invert
  (lambda (lst)
    (let loop ([lst lst])
      (if (null? lst) '()
          (let ([inner-list (car lst)])
            (append (list(list (cadr inner-list) (car inner-list))) (loop (cdr lst))))))))


(define combine-consec
  (lambda (lst)
    (if (null? lst) '()
    (let loop ([lst (cdr lst)]
               [start (car lst)]
               [current (car lst)])
      (cond
         ((null? lst)
          (list (list start current)))
         ((= (+ current 1) (car lst))
          (loop (cdr lst) start (car lst)))
         (else
          (append (list(list start current)) (loop (cdr lst) (car lst) (car lst)))))))))
    ;      (loop (cdr lst) (car lst) (car lst) (append (list(list start current)) result))))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
