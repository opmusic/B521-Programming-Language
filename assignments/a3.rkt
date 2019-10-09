#lang racket

(require racket/trace)

#|Part 1: Interpreters and Environments|#

(define value-of
  (lambda (e env)
    (match e
      [`,n
        #:when (natural? n)
        n]
      [`,b
        #:when (boolean? b)
        b]
      [`(zero? ,n)
        (zero? (value-of n env))]
      [`(sub1 ,n)
        (sub1 (value-of n env))]
      [`(* ,x ,y)
        (* (value-of x env) (value-of y env))]
      [`(if ,c ,b1 ,b2)
        (if (value-of c env) (value-of b1 env) (value-of b2 env))]
      [`(let ([,x ,y]) ,body)
        #:when (symbol? x)
        (let ([tmp (box (value-of y env))])
            (value-of body (lambda (arg) (if (eqv? x arg) tmp (env arg)))))]
      [`(set! ,x ,y)
        #:when (symbol? x)
        ;(lambda (arg) (if (eqv? x arg) (value-of y env) (env arg)))]
        (set-box! (env x) (value-of y env))]
      [`(begin2 ,exp1 ,exp2)
        (begin (value-of exp1 env) (value-of exp2 env))]
      [`,y
        #:when (symbol? y)
        (unbox (env y))]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (lambda (arg) (value-of body (let ([tmp (box arg)])
                                       (lambda (y) (if (eqv? y x) tmp (env y))))))]
      [`(,rator ,rand)
        ((value-of rator env) (value-of rand env))])))

(define empty-env
  (lambda () '()))

(define value-of-fn
  (lambda (e env)
    (match e
      [`,n
        #:when (natural? n)
        n]
      [`,b
        #:when (boolean? b)
        b]
      [`(zero? ,n)
        (zero? (value-of-fn n env))]
      [`(sub1 ,n)
        (sub1 (value-of-fn n env))]
      [`(* ,x ,y)
        (* (value-of-fn x env) (value-of-fn y env))]
      [`(if ,c ,b1 ,b2)
        (if (value-of-fn c env) (value-of-fn b1 env) (value-of-fn b2 env))]
      [`(let ([,x ,y]) ,body)
        #:when (symbol? x)
        (value-of-fn body (extend-env-fn env x (value-of-fn y env)))]
      [`,y
        #:when (symbol? y)
        (apply-env-fn env y)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (lambda (arg) (value-of-fn body (extend-env-fn env x arg)))]
      [`(,rator ,rand)
        ((value-of-fn rator env) (value-of-fn rand env))])))

(define empty-env-fn
  (lambda ()
    (lambda (y)
      (error "empty env"))))

(define apply-env-fn
  (lambda (env y)
    (env y)))

(define extend-env-fn
  (lambda (env x arg)
    (lambda (y)
      (if (eqv? y x) arg (apply-env-fn env y)))))

(define value-of-ds
  (lambda (e env)
    (match e
      [`,n
       #:when (natural? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,n)
       (zero? (value-of-ds n env))]
      [`(sub1 ,n)
       (sub1 (value-of-ds n env))]
      [`(* ,x ,y)
       (* (value-of-ds x env) (value-of-ds y env))]
      [`(if ,c ,b1 ,b2)
       (if (value-of-ds c env) (value-of-ds b1 env) (value-of-ds b2 env))]
      [`(let ([,x ,y]) ,body)
       #:when (symbol? x)
       (value-of-ds body (extend-env-ds env x (value-of-ds y env)))]
      [`,y
       #:when (symbol? y)
       (apply-env-ds env y)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (lambda (arg) (value-of-ds body (extend-env-ds env x arg)))]
      [`(,rator ,rand)
       ((value-of-ds rator env) (value-of-ds rand env))])))

(define empty-env-ds
  (lambda ()
    '()))

(define apply-env-ds
  (lambda (env y)
    (match env
      ['() (error "empty env")]
      [`((,x . ,arg) . ,env)
       (if (eqv? y x) arg (apply-env-ds env y))])))

(define extend-env-ds
  (lambda (env x arg)
    `((,x . ,arg) . ,env)))

#|Part 2 "fo-eulav"|#

(define fo-eulav
  (lambda (e env)
    (match e
      [`,n
        #:when (natural? n)
        n]
      [`,b
        #:when (boolean? b)
        b]
      [`(,n ?orez)
        (zero? (fo-eulav n env))]
      [`(,n 1bus)
        (sub1 (fo-eulav n env))]
      [`(,x ,y *)
        (* (fo-eulav x env) (fo-eulav y env))]
      [`(,b2 ,b1 ,c fi)
        (if (fo-eulav c env) (fo-eulav b1 env) (fo-eulav b2 env))]
      [`(,body ([,y ,x]) tel)
        #:when (symbol? x)
        (fo-eulav body (lambda (z) (if (eqv? z x) (fo-eulav y env) (env z))))]
      [`,y
        #:when (symbol? y)
        (env y)]
      [`(,body (,x) adbmal)
        #:when (symbol? x)
        (lambda (arg) (fo-eulav body (lambda (y) (if (eqv? y x) arg (env y)))))]
      [`(,rand ,rator)
        ((fo-eulav rator env) (fo-eulav rand env))])))

#|Brainteasers|#
#|5. see above value-of|#
#|ref: https://zhuanlan.zhihu.com/p/34442630|#

#|6.|#
(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))

(define empty-env-lex
  (lambda () '()))

(define apply-env-lex list-ref)

(define extend-env-lex cons)

#|Just Dessert|#
#|ref: https://en.wikipedia.org/wiki/Church_encoding|#
#|7.|#
(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(define c+ (lambda (m)
             (lambda (n)
               (lambda (a) (lambda (b) ((m a) ((n a) b)))))))

(define csub1 (lambda (n)
                (lambda (f)
                  (lambda (x)
                    (((n
                      (lambda (a) (lambda (b) (b (a f)))))
                        (lambda (y) x))
                         (lambda (y) y))))))
