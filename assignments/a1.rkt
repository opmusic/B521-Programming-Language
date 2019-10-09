#lang racket

#|1. countdown|#
(define countdown
  (lambda (n)
    (cond
      ((zero? n) (list 0))
      (else (cons n (countdown (sub1 n)))))))

#|2. insertR|#
(define insertR
  (lambda (x y ls)
    (cond
      ((null? ls) '())
      ((eqv? x (car ls))  (cons x (cons y (insertR x y (cdr ls)))))
      (else (cons (car ls) (insertR x y (cdr ls)))))))

#|3. remv-1st|#
(define remv-1st
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((eqv? x (car ls)) (cdr ls))
      (else (cons (car ls) (remv-1st x (cdr ls)))))))

#|4. list-index-ofv|#
(define list-index-ofv?
  (lambda (x ls)
    (cond
      ((null? ls) 0)
      ((eqv? x (car ls)) 0)
      (else (add1 (list-index-ofv? x (cdr ls)))))))

#|5. filter|#
(define filter
  (lambda (pred ls)
    (cond
      ((null? ls) '())
      ((pred (car ls)) (cons (car ls) (filter pred (cdr ls))))
      (else (filter pred (cdr ls))))))

#|6. zip|#
(define zip
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) '())
      ((null? ls2) '())
      (else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))))))

#|7. map|#
(define map
  (lambda (pred ls)
    (cond
      ((null? ls) '())
      (else (cons (pred (car ls)) (map pred (cdr ls)))))))

#|8. append|#
(define append
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) ls2)
      (else (cons (car ls1) (append (cdr ls1) ls2))))))

#|9. reverse|#
(define reverse
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (append (reverse (cdr ls)) (list (car ls)))))))

#|10. fact|#
(define fact
  (lambda (n)
    (cond
      ((zero? n) 1)
      (else (* n (fact (sub1 n)))))))

#|11. memv|#
(define memv
  (lambda (n ls)
    (cond
      ((null? ls) #f)
      ((eqv? n (car ls)) ls)
      (else (memv n (cdr ls))))))

#|12. fib|#
(define fib
  (lambda (n)
    (cond
      ((zero? n) 0)
      ((eqv? 1 n) 1)
      (else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))))

#|13. ((w . (x . ())) y (z . ()))|#

#|14. binary->natural|#
(define binary->natural
  (lambda (ls)
    (cond
      ((null? ls) 0)
      ((zero? (car ls)) (* 2 (binary->natural (cdr ls))))
      (else (add1 (* 2 (binary->natural (cdr ls))))))))

#|15. minus|#
(define minus
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (minus x (sub1 y)))))))

#|16. div|#
(define div
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) 0)
      (else (add1 (div (minus x y) y))))))

#|17. append-map|#
(define append-map
  (lambda (pred ls)
    (cond
      ((null? ls) '())
      (else (append (pred (car ls)) (append-map pred (cdr ls)))))))

#|18. set-difference|#
#|(define mem?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      (else (or (eqv? (car ls) x)
                (mem? x (cdr ls)))))))

(define set-difference
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) '())
      ((null? ls2) ls1)
      ((mem? (car ls1) ls2) (set-difference (cdr ls1) ls2))
      (else (cons (car ls1) (set-difference (cdr ls1) ls2))))))|#

(define set-difference
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) '())
      ((null? ls2) ls1)
      (else (set-difference (remv-1st (car ls2) ls1) (cdr ls2))))))

#|19. powerset
ref: https://stackoverflow.com/questions/20622945/how-to-do-a-powerset-in-drracket|#

(define powerset
  (lambda (ls)
    (cond
      ((null? ls) '(()))
      (else (append-map (lambda (x)
                          (list x (cons (car ls) x)))
                        (powerset (cdr ls)))))))

#|20. cartesian-product|#
(define cartesian-product
  (lambda (ls)
    (cond
      ((null? ls) '(()))
      (else (append-map (lambda (y)
                          (map (lambda (x)
                                 (cons x y))
                               (car ls)))
                        (cartesian-product (cdr ls)))))))

#|21. foldr|#
#|insertR-fr|#
(define insertR-fr
  (lambda (x y ls)
    (foldr
     (lambda (e rls)
       (cond
         ((eqv? x e) (cons x (cons y rls)))
         (else (cons e rls))))
     '() ls)))

#|filter-fr|#
(define filter-fr
  (lambda (pred ls)
    (foldr
     (lambda (e lsr)
       (cond
         ((pred e) (cons e lsr))
         (else lsr)))
     '() ls)))

#|map-fr|#
(define map-fr
  (lambda (pred ls)
    (foldr
     (lambda (e lsr)
       (cons (pred e) lsr))
     '() ls)))

#|append-fr|#
(define append-fr
  (lambda (ls1 ls2)
    (foldr
     (lambda (e lsr)
       (cons e lsr))
     ls2 ls1)))

#|reverse-fr|#
(define reverse-fr
  (lambda (ls)
    (foldr
     (lambda (e lsr)
       (append lsr (list e)))
       '() ls)))

#|binary->natural-fr|#
(define binary->natural-fr
  (lambda (ls)
    (foldr
     (lambda (e res)
       (cond
         ((zero? e) (* 2 res))
         (else (add1 (* 2 res)))))
     0 ls)))

#|append-map-fr|#
(define append-map-fr
  (lambda (pred ls)
    (foldr
     (lambda (e lsr)
      (append (pred e) lsr))
     '() ls)))

#|set-difference-fr|#
(define set-difference-fr
  (lambda (ls1 ls2)
    (foldr
     (lambda (e lsr)
       (remv-1st e lsr))
     ls1 ls2)))

#|powerset-fr|#
(define powerset-fr
  (lambda (ls)
    (foldr
     (lambda (e lsr)
       (append-map-fr (lambda (x)
                        (list x (cons e x))) lsr))
      '(()) ls)))

#|cartesian-product-fr|#
(define cartesian-product-fr
  (lambda (ls)
    (foldr
     (lambda (e lsr)
       (append-map-fr (lambda (y)
                          (map-fr (lambda (x)
                                    (cons x y))
                                  e))
                      lsr))
     '(()) ls)))

#|22. Collatz Conjecture|#
(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
    (one-case (even-case (odd-case base)))
    ;; this should be a single line, without lambda
    ))

#|Just Dessert
21. |#
(define quine ((lambda (x) (list x (list 'quote x)))
  '(lambda (x) (list x (list 'quote x)))))
