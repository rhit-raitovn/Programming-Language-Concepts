#lang racket

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right BST-insert-nodes BST-contains? BST-height let->application let*->let qsort sort-list-of-symbols)

;1
(define curry2
  (lambda (proc)
    (lambda (a)
      (lambda (b)
        (proc a b)))))

;2
(define curried-compose
  (lambda (proc)
    (lambda (a)
      (lambda (b)
         (proc (a b))))))

; 3 ((compose car car) '((1 7) (2 9))
(define compose
  (lambda lst
    (if (null? lst)
        (lambda (a) a)
        (lambda (a)
          (let helper ([lst lst] [a a])
             (if (null? lst) a
             ((car lst) (helper (cdr lst) a))))))))

;4
(define make-list-c
  (lambda (num)
    (lambda (str)
      (let repeat ([num num])
        (if (= num 0)
            '()
            (append (list str) (repeat (- num 1)))) ))))

;5
(define reverse-it
  (lambda (lst)
    (let loop ([lst lst])
      (if (null? lst) '()
          (append (reverse-it (cdr lst)) (list (car lst)))))))

;6
(define map-by-position
  (lambda (fn-list arg-list)
    (map map-helper fn-list arg-list)))

(define map-helper
  (lambda (x y)
    (x y)))           


;7 - got help from TAs
 
;DONE
(define empty-BST
  (lambda () '()))

;DONE
(define empty-BST?
  (lambda (obj)
    (if (eq? obj (empty-BST)) #t #f)))

;DONE
(define BST-insert
  (lambda (num bst)
    (cond
      [(empty-BST? bst) (list num '() '())]   ;with empty left and right
      [(= (car bst) num) bst]
      [(< num (car bst))
       (list (car bst)
             (BST-insert num (BST-left bst))
             (BST-right bst))]
      [(> num (car bst))
       (list (car bst)
             (BST-left bst)
             (BST-insert num (BST-right bst)))])))

;DONE
(define BST-inorder-helper
  (lambda (bst)
    (cond
      [(empty-BST? bst) '()]
      [(null? (cdr bst)) (list (car bst))]
      [else
       (append (BST-inorder-helper (BST-left bst))
               (list (car bst))
               (BST-inorder-helper (BST-right bst)))])))

(define BST-inorder
  (lambda (bst)
    (cond
      [(empty-BST? bst) '()]  ; no elements
      [(null? (cdr bst)) bst] ; one element only
      [else (append (BST-inorder-helper (BST-left bst))
                    (list (car bst))
                    (BST-inorder-helper (BST-right bst)))])))

;DONE
(define BST-helper?
  (lambda (bst)
    (if (list? bst)
        (cond
          [(equal? (empty-BST) bst) #t]
          [(not (= (length bst) 3)) #f]
          [else
           (let ([root (car bst)]
                 [left (cadr bst)]
                 [right (caddr bst)])
             (and
              (number? root)
              (or (equal? left (empty-BST))
                  (and (list? left)
                       (< (car left) root)
                       (BST-helper? left)))
              (or (equal? right (empty-BST))
                  (and (list? right)
                       (> (car right) root)
                       (BST-helper? right)))))]) 
        #f)))

; DONE
(define BST?
  (lambda (bst)
    (if (list? bst)
        (if (BST-helper? bst)
            (let loop ([lst (BST-inorder bst)])
              (cond
                [(null? lst) #t]
                [(not (list? lst)) #t]
                [(null? (cdr lst)) #t]
                [else (if (> (car lst) (cadr lst))
                          #f
                          (loop (cdr lst)))]))
              #f)
        #f)))

;DONE
(define BST-element
  (lambda (bst) (car bst)))

;DONE
(define BST-left
  (lambda (bst) (cadr bst)))

;DONE
(define BST-right
  (lambda (bst) (caddr bst)))

;DONE
(define BST-insert-nodes
  (lambda (bst nums)
    (if (null? nums) bst
        (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

;DONE 
(define BST-contains?
  (lambda (bst num)
    (cond
      [(empty-BST? bst) #f]
      [(< num (car bst))
       (if (null? (cdr bst)) #f (BST-contains? (BST-left bst) num))]
      [(> num (car bst))
       (if (null? (cdr bst)) #f (BST-contains? (BST-right bst) num))]
      [(= num (car bst)) #t]
      [else #f])))

;DONE
(define BST-height
  (lambda (bst)
    (cond
      [(empty-BST? bst) -1]
      [(null? (cdr bst)) 0]
      [(null? (BST-right bst)) (+ 1 (BST-height (BST-left bst)))]
      [(null? (BST-left bst)) (+ 1 (BST-height (BST-right bst)))]
      [else (+ 1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst))))])))






;8
;DONE
(define let->application
  (lambda (code)
    (if (null? code) code
        (if (eq? (car code) 'let)
            (let* ([let-values (cadr code)]
                   [variables (map car let-values)]
                   [values (map cadr let-values)]
                   [body (cddr code)])
              (cons (cons 'lambda (cons variables body)) values))
            code))))

;DONE
(define let*->let
  (lambda (proc)
    (let-helper (cadr proc) proc)))

(define let-helper
  (lambda (lst proc)
    (if (null? (cdr lst))
        (list 'let (list (car lst)) (caddr proc))
        (list 'let (list (car lst)) (let-helper (cdr lst) proc)))))

;DONE
(define qsort
  (lambda (proc lst)
    (if (<= (length lst) 1)
        lst
        (let* ([pivot (list-ref lst 0)]
              [less (qsort-helper-1 proc (cdr lst) #t pivot)]
              [greater (qsort-helper-1 proc (cdr lst) #f pivot)])
          (append (qsort proc less) (list pivot) (qsort proc greater))
         ))))

(define qsort-helper-1
  (lambda (proc lst b pivot)
    (let loop ([lst lst] [result '()])
      (if (null? lst)
          (reverse result)
          (let ([current (car lst)])
            (loop (cdr lst)
                  (if (eq? (proc current pivot) b)
                      (cons current result)
                      result)))))))

;DONE
(define sort-list-of-symbols
  (lambda (los)
    (map string->symbol (sort (map symbol->string los) string<?))))


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
