#lang racket
(require racket/trace)

(provide vector-append-list group-by-two group-by-n bt-leaf-sum bt-inorder-list bt-max bt-max-interior slist-map slist-reverse slist-paren-count slist-depth slist-symbols-at-depth path-to make-c...r)

(define vector-append-list
  (lambda (a b)
    (nyi)))

;#1 DONE
(define group-by-two
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(<= (length ls) 2) (list ls)]
      [(append (list (list (car ls) (cadr ls))) (group-by-two (cddr ls)))])))

;#2 DONE
(define group-by-n
  (lambda (ls n)
    (cond
      [(null? ls) '()]
      [(<= (length ls) n) (list ls)]
      [(append (list (group-by-n-helper ls n)) (group-by-n (list-tail ls n) n))])))


(define group-by-n-helper
  (lambda (ls n)
    (cond
      [(null? ls) '()]
      [(= n 0) '()]
      [(append (list (car ls)) (group-by-n-helper (cdr ls) (- n 1)))])))

;#3 DONE
(define bt-leaf-sum
  (lambda (t)
    (cond [(null? t) 0]
          [(number? t) t]
          [(+ (bt-leaf-sum (cadr t)) (bt-leaf-sum (caddr t)))]
          )))
          
(define bt-inorder-list
  (lambda (t)
    (if (not(pair? t)) '()
    (let ([left (cadr t)]
          [right (caddr t)])
      (cond
        [(and (number? left) (number? right)) (list(car t))]
        [(and (pair? left) (number? right))
         (append (bt-inorder-list left) (list (car t)))]
        [(and (number? left) (pair? right))
         (append (list (car t))(bt-inorder-list right))]
        [(append (bt-inorder-list left)(list (car t)) (bt-inorder-list right))])))))

(define bt-max
  (lambda (t)
    (cond [(null? t) 0]
          [(number? t) t]
          [(max (bt-max (cadr t)) (bt-max (caddr t)))]
          )))

(define bt-max-interior
  (lambda (t)
    (car (interior-helper t))))

(define interior-helper 
  (lambda (t)
    (cond [(and (number? (cadr t))
                (number? (caddr t)))
           ;(list (car t) (+ (cadr t) (caddr t)))]
          (list (car t) (max (cadr t) (caddr t) (+ (cadr t) (caddr t))) (+ (cadr t) (caddr t)))]
          
          [(and (number? (caddr t)) (pair? (cadr t)))
           (let* ([left-tree (interior-helper (cadr t))]
                 [right-value (caddr t)]
                 [sum (+ (caddr left-tree) right-value)])
             (cond
               [(equal? (max sum (cadr left-tree)) sum)
                (list (car t) sum sum)]
               [(equal? (max sum (cadr left-tree)) (cadr left-tree))
                (list (car left-tree) (cadr left-tree) sum)]
               )	
             )]

          [(and (number? (cadr t)) (pair? (caddr t)))
           (let* ([right-tree (interior-helper (caddr t))]
                  [left-value (cadr t)]
                  [sum (+ (caddr right-tree) left-value)])
             (cond
               [(equal? (max sum (cadr right-tree)) sum)
                (list (car t) sum sum)]
               [(equal? (max sum (cadr right-tree)) (cadr right-tree))
                (list (car right-tree) (cadr right-tree) sum)]
               )	
             )]
          
          [else
           (let* ([left-tree (interior-helper (cadr t))]
                 [right-tree (interior-helper (caddr t))]
                 [sum (+ (caddr left-tree) (caddr right-tree))])
                 (cond
                   [(equal? (max (cadr right-tree) sum (cadr left-tree)) sum) (list (car t) sum sum)]
                   [(equal? (max (cadr right-tree) sum (cadr left-tree)) (cadr left-tree)) (list (car left-tree) (cadr left-tree) sum)]
                   [(equal? (max (cadr right-tree) sum (cadr left-tree)) (cadr right-tree)) (list (car right-tree) (cadr right-tree) sum)]
                   ))])))

;(trace interior-helper)                 

;7B 
(define slist-map
  (lambda (proc slist)
    (cond [(null? slist) '()]
          [(list? (car slist))
           (append (list (slist-map proc (car slist))) (slist-map proc (cdr slist)))]
          [(symbol? (car slist))(append (list (proc (car slist))) (slist-map proc (cdr slist)))]
          )))

(define slist-reverse
  (lambda (slist)
    (cond [(null? slist) '()]
          [(list? (car slist))
           (append (slist-reverse (cdr slist)) (list (slist-reverse (car slist))))  ]     
          [(append (slist-reverse (cdr slist)) (list (car slist)))]
          )))

;(trace slist-reverse)

(define slist-paren-count
  (lambda (slist)
    (cond [(null? slist) 2]
          [(list? (car slist)) (+ (slist-paren-count (cdr slist)) (slist-paren-count (car slist)))]
          [(slist-paren-count (cdr slist))]
          )))

(define slist-depth
  (lambda (slist)
    (cond [(null? slist) 1]
          [(list? (car slist))
           (if (list? (cdr slist))
               (max (+ 1 (slist-depth (car slist))) (slist-depth (cdr slist)))
               (+ 1 (slist-depth (car slist))))]
          [(slist-depth (cdr slist))]
          )))

;(trace slist-depth)

(define slist-symbols-at-depth
  (lambda (slist n)
    (cond [(null? slist) '()]
          [(= n 0) '()]
          [(= n 1)
           (if (list? (car slist))
               (slist-symbols-at-depth (cdr slist) n)
               (cons (car slist) (slist-symbols-at-depth (cdr slist) n))) ]
          [(if (list? (car slist))
               (append (slist-symbols-at-depth (car slist) (- n 1)) (slist-symbols-at-depth (cdr slist) n))
               (slist-symbols-at-depth (cdr slist) n))])))
           
(define path-to
  (lambda (slist sym)
    (cond [(null? slist) #f]
          [(eq? sym (car slist)) (list 'car)]
          [(list? (car slist))
           (let ([in (path-to (car slist) sym)])
             (if in
                 (cons 'car in)
                 (let ([in-right (path-to (cdr slist) sym)])
                   (if in-right
                       (cons 'cdr in-right)
                       #f))))]
          [else
           (let ([in-next (path-to (cdr slist) sym)])
             (if in-next
                 (cons 'cdr in-next)
                 #f))]
          )))

(define make-c...r
  (lambda (carstr)
    (lambda (lst)
    (let loop ([clist (string->list carstr)]
               [lst lst])
      (cond [(null? lst) '()]
            [(null? clist) lst]
            [(eq? (car clist) #\a) (car ((make-c...r (substring carstr 1))lst))]
            [(cdr ((make-c...r (substring carstr 1))lst))]
      )))))
;(trace make-c...r)

;(define caddddr (make-c...r "adddd"))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
