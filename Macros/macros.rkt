#lang racket

; only functions in racket/base can be used by default in macros
; this adds some other useful prodcedures
(require (for-syntax racket/list))
(require racket/trace)

(provide init all-equal begin-unless range-cases for)

(define-syntax init
  (syntax-rules ()
    ((_ e1 ...) (begin (define e1 0) ...))))

(define-syntax all-equal
  (syntax-rules ()
    [(_) #t]     ; no args - #t
    [(_ e) #t]             ; one arg - #t
    [(_ e1 e2) (equal? e1 e2)]     ; two arguments - directly compare
    [(_ e1 e2 e3 ...)
     (let ([t1 e1] [t2 e2])        
       (and (equal? t1 t2)             ; compare e1 and e2
            (all-equal t2 e3 ...)))])) ; recurse the rest

(define-syntax begin-unless
  (syntax-rules ()
    [(_ cond) cond]                             
    [(_ cond body)(if cond cond (begin body cond))]                   
    [(_ cond body1 body2 ...) 
     (if cond cond 
         (begin 
           body1
           (begin-unless cond body2 ...)))]))


(define-syntax range-cases
  (syntax-rules (< else)
    ;; base case: one < and else
    [(_ var (< value result) (else else-result))
     (let ([v var])                
       (if (< v value)
           result
           else-result))]
    ;; multiples
    [(_ var (< value result) rest ...)
     (let ([v var])               
       (if (< v value)
           result
           (range-cases v rest ...)))]))


(define-syntax for
  (syntax-rules (:)
    ;; one var
    [(_ (init : cond : update ...) body ...)
     (begin
       init                  ; initialization
       (let loop ()              ; loop
         (if cond                ; check the condition
             (begin
               body ...            ; execute the body
               update ...               ; update
               (loop))                  ; back to the loop
             (void))))]
    [(_ ((begin init ...) : cond : update ...) body ...)
     (begin
       init ...                       
       (let loop ()                   
         (if cond        
             (begin
               body ...            
               update ...          
               (loop))      
             (void))))]         
    ))              


;(let ([sum 0][i 0])
;  (for ((begin (set! sum 0) (set! i 1)) : (< i 10) : (set! i (+ i 1)))
;    (set! sum (+ i sum)))
;  sum)

;(let ([a 1] [ls '()])
;  (for ((set! a 42) : (< a 45) : (set! a (+ a 1))) 
;    (set! ls (cons a ls)))
;  ls)




