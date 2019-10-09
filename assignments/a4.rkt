#lang racket
(require racket/trace)

#|Part 1|#

(define empty-env
  (lambda ()
    '()))

(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (env x arg)
    (lambda (y)
      (if (eqv? y x) arg (apply-env env y)))))

(define apply-closure
  (lambda (exp val)
    (exp val)))

(define make-closure-cbv
  (lambda (x body env)
    (lambda (arg)
      (val-of-cbv body (extend-env env x arg)))))

(define val-of-cbv
  (lambda (exp env)
    (match exp
      [''() '()] ;empty list
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(null? ,ls) (null? (val-of-cbv ls env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(cons ,ls1 ,ls2) (cons (val-of-cbv ls1 env) (val-of-cbv ls2 env))]
      [`(car ,ls) (car (val-of-cbv ls env))]
      [`(cdr ,ls) (cdr (val-of-cbv ls env))]
      [`(cons^ ,ls1 ,ls2) (cons (lambda () (val-of-cbv ls1 env)) (lambda () (val-of-cbv ls2 env)))]
      [`(car^ ,ls) ((car (val-of-cbv ls env)))]
      [`(cdr^ ,ls) ((cdr (val-of-cbv ls env)))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(set! ,x ,e) (set! x (val-of-cbv e env))]
      [`(let ([,x ,y]) ,body)
       #:when (symbol? x) (val-of-cbv body (extend-env env x (val-of-cbv y env)))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (val-of-cbv rand env))])))

(define make-closure-cbr
  (lambda (x body env)
    (lambda (arg)
      (val-of-cbr body (extend-env env x arg)))))

(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                    (val-of-cbr conseq env)
                                    (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(set! ,x ,e) (set-box! (apply-env env x) (val-of-cbr e env))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      [`(,rator ,x) #:when (symbol? x) ((val-of-cbr rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                     (box (val-of-cbr rand env)))])))

(define make-closure-cbname
  (lambda (x body env)
    (lambda (arg)
      (val-of-cbname body (extend-env env x arg)))))

(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(set! ,x ,e) (set! x (val-of-cbname e env))]
      [`,y #:when (symbol? y) ((apply-env env y))]
      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                     (lambda () (val-of-cbname rand env)))])))

(define make-closure-cbneed
  (lambda (x body env)
    (lambda (arg)
      (val-of-cbneed body (extend-env env x arg)))))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(set! ,x ,e) (set-box! (apply-env env x) (val-of-cbneed e env))]
      [`,y #:when (symbol? y) (let ([b (apply-env env y)])
                                (let ([th (unbox b)])
                                  (let ([v (th)])
                                    (begin (set-box! b (lambda () v))
                                           v))))]
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      [`(,rator ,x) #:when (symbol? x) ((val-of-cbr rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (lambda () (val-of-cbneed rand env))))])))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))

#|Brainteaser|#
#|see val-of-cbv|#
;(trace val-of-cbname)
(define cons-test
    '(let ((fix (lambda (f)
                 ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f)
                            (lambda (l)
                               (if (null? l)
                                   '()
                                   (cons^ (f (car^ l))
                                          ((map f) (cdr^ l))))))))))
          (let ((take (fix (lambda (take)
                             (lambda (l)
                               (lambda (n)
                                 (if (zero? n)
                                     '()
                                      (cons (car^ l) 
                                            ((take (cdr^ l)) (sub1 n))))))))))
            ((take ((fix (lambda (m)
                           (lambda (i)
                             (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))

#|Just Dessert|#