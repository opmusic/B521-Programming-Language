#lang racket

#| Part 1: Natural Recursion Refresher|#
#|1. list-ref|#
(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
        (lambda (n)
          ;; complete the definition
          (cond
            ((null? ls) #f)
            ((zero? n) ls)
            (else (cdr (nth-cdr (sub1 n)))))
        )))
      (car (nth-cdr n)))))

#|2. union|#
(define union
  (lambda (ls1 ls2)
    (cond
      ((null? ls2) ls1)
      ((memv (car ls2) ls1) (union ls1 (cdr ls2)))
      (else (append (union ls1 (cdr ls2)) (list (car ls2)))))))

#|3. extend|#
(define (extend x pred)
  (lambda (n)
    (cond
      ((eqv? x n) #t)
      ((pred n) #t)
      (else #f))))

#|4. walk-symbol|#
(define walk-symbol
  (lambda (x ls)
    (cond
      ((assv x ls) (walk-symbol (cdr (assv x ls)) ls))
      (else x))
  ))

#| Part 2: Free, Bound, Lexical Address|#
#|5. lambda->lumbda|#
(define lambda->lumbda
  (lambda (e)
    (match e
      [`,y
        #:when (symbol? y)
        y]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        `(lumbda (,x) ,(lambda->lumbda body))]
      [`(,rator ,rand)
        `(,(lambda->lumbda rator) ,(lambda->lumbda rand))])))

#|6. var-occurs?|#
(define var-occurs?
  (lambda (var e)
    (match e
      [`,y
        #:when (symbol? y)
        (eqv? y var)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (var-occurs? var body)]
      [`(,rator ,rand)
        (or (var-occurs? var rator) (var-occurs? var rand))])))

#|7. vars|#
(define vars
  (lambda (e)
    (match e
      [`,y
        #:when (symbol? y)
        (list y)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (vars body)]
      [`(,rator ,rand)
        (append (vars rator) (vars rand))])))

#|8. unique-vars|#
(define unique-vars
  (lambda (e)
    (match e
      [`,y
        #:when (symbol? y)
        (list y)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (unique-vars body)]
      [`(,rator ,rand)
        (union (unique-vars rator) (unique-vars rand))])))

#|9. var-occurs-free?|#
(define var-occurs-free?
  (lambda (var e)
    (match e
      [`,y
        #:when (symbol? y)
        (eqv? y var)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (and (not (eqv? x var)) (var-occurs-free? var body))]
      [`(,rator ,rand)
        (or (var-occurs-free? var rator) (var-occurs-free? var rand))])))

#|10. var-occurs-bound?|#
(define var-occurs-bound?
  (lambda (var e)
    (match e
      [`,y
        #:when (symbol? y)
        #f]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (or (and (eqv? x var) (var-occurs-free? var body))
            (var-occurs-bound? var body))]
      [`(,rator ,rand)
        (or (var-occurs-bound? var rator) (var-occurs-bound? var rand))])))

#|11. unique-free-vars|#
(define remv-1st
  (lambda (x ls)
    (cond
      ((null? ls) ls)
      ((eqv? x (car ls)) (cdr ls))
      (else (cons (car ls) (remv-1st x (cdr ls)))))))

(define unique-free-vars
  (lambda (e)
    (match e
      [`,y
        #:when (symbol? y)
        (list y)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (remv-1st x (unique-free-vars body))]
      [`(,rator ,rand)
        (union (unique-free-vars rator) (unique-free-vars rand))])))

#|12. unique-bound-vars|#
(define unique-bound-vars
  (lambda (e)
    (match e
      [`,y
        #:when (symbol? y)
        '()]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (if (var-occurs-free? x body)
            (union (list x) (unique-bound-vars body))
            (unique-bound-vars body))]
      [`(,rator ,rand)
        (union (unique-bound-vars rator) (unique-bound-vars rand))])))

#|13. lex|#
(define index-of
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((eqv? x (car ls)) 0)
      (else (add1 (index-of x (cdr ls)))))))

(define lex
  (lambda (e cenv)
    (match e
      [`,y
        #:when (symbol? y)
        (if (memv y cenv) (list 'var (index-of y cenv)) #f)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        `(lambda ,(lex body (cons x cenv)))]
      [`(,rator ,rand)
       `(,(lex rator cenv) ,(lex rand cenv))])))

#|14. walk-symbol-update|#
(define walk-symbol-update
  (lambda (x ls)
    (cond
      ((assv x ls)
       (let ([v (walk-symbol-update (unbox (cdr (assv x ls))) ls)])
         (set-box! (cdr (assv x ls)) v)
         v))
      (else x))))

#|15. var-occurs-both|#
(define var-occurs-both?
  (lambda (var e)
    (match e
      [`,y
        #:when (symbol? y)
        (values (eqv? y var)
                #f)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (let-values ([(p q) (var-occurs-both? var body)])
                     (values (and (not (eqv? x var)) p)
                             (or (and (eqv? x var) p) q)))]
      [`(,rator ,rand)
        (let-values ([(p1 q1) (var-occurs-both? var rator)]
                     [(p2 q2) (var-occurs-both? var rand)])
          (values (or p1 p2) (or q1 q2)))])))