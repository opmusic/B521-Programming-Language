#lang racket
(require racket/trace)

#|Part I|#
(define lex
  (lambda (e cenv)
    (match e
      [`,n #:when (number? n) `(const ,n)]
      [`(zero? ,n) `(zero? ,(lex n cenv))]
      [`(sub1 ,n) `(sub1 ,(lex n cenv))]
      [`(* ,x ,y) `(* ,(lex x cenv) ,(lex y cenv))]
      [`(if ,c ,b1 ,b2) `(if ,(lex c cenv) ,(lex b1 cenv) ,(lex b2 cenv))]
      [`(let ((,x ,y)) ,body)
        #:when (symbol? x)
        `(let ,(lex y cenv) ,(lex body (cons x cenv)))]
      [`,y
        #:when (symbol? y)
        (if (memv y cenv) (list 'var (index-of cenv y)) #f)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        `(lambda ,(lex body (cons x cenv)))]
      [`(,rator ,rand)
       `(,(lex rator cenv) ,(lex rand cenv))])))

#|Part II|#
(define empty-env
  (lambda () '()))

(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (env x arg)
    (lambda (y)
      (if (eqv? y x) arg (apply-env env y)))))


(define closure-fn
  (lambda (x body env)
    (lambda (arg)
      (value-of-fn body (extend-env env x arg)))))

(define apply-closure-fn
  (lambda (exp val)
    (exp val)))

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
        (value-of-fn body (extend-env env x (value-of-fn y env)))]
      [`,y
        #:when (symbol? y)
        (apply-env env y)]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        (closure-fn x body env)]
      [`(,rator ,rand)
        (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))])))

(define closure-ds
  (lambda (x body env)
    `((,x . ,body) . ,env)))

(define apply-closure-ds
  (lambda (exp val)
    (match exp
      [`() (error "empty env")]
      [`((,x . ,body) . ,env) (value-of-ds body (extend-env env x val))])))

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
       (value-of-ds body (extend-env env x (value-of-ds y env)))]
      [`,y
       #:when (symbol? y)
       (apply-env env y)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (closure-ds x body env)]
      [`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))

#|Part III|#
#;(define value-of-dynamic-env
  (lambda (e env)
    (match e
      [`,n #:when (number? n) `(,n ,env)]
      [`,b #:when (boolean? b) `(,b ,env)]
      [`(quote ,v) `(,v ,env)]
      [`(sub1 ,n)
        (match (value-of-dynamic-env n env)
          [`(,v ,env) `(,(sub1 v) ,env)])]
      [`(* ,m ,n)
        (match (value-of-dynamic-env m env)
          [`(,x ,env)
            (match (value-of-dynamic-env n env)
              [`(,y ,env) `(,(* x y) ,env)])])]
      [`(zero? ,n)
        (match (value-of-dynamic-env n env)
          [`(,v ,env) `(,(zero? v) ,env)])]
      [`(null? ,e)
        (match (value-of-dynamic-env e env)
          [`(,v ,env) `(,(null? v) ,env)])]
      [`(car ,e)
        (match (value-of-dynamic-env e env)
          [`(,ls ,env) `(,(car ls) ,env)])]
      [`(cdr ,e)
        (match (value-of-dynamic-env e env)
          [`(,ls ,env) `(,(cdr ls) ,env)])]
      [`(cons ,a ,d)
        (match (value-of-dynamic-env a env)
          [`(,a ,env)
            (match (value-of-dynamic-env d env)
              [`(,d ,env) `((,a . ,d) ,env)])])]
      [`(if ,test ,then ,else)
        (match (value-of-dynamic-env test env)
          [`(,test ,env)
            (if test
              (value-of-dynamic-env then env)
              (value-of-dynamic-env else env))])]
      [`(let ([,x ,y]) ,body)
        (match (value-of-dynamic-env y env)
          [`(,a ,env) `(,(value-of-dynamic-env body (extend-env env x a)) ,env)])]
      [`,y #:when (symbol? y) `(,(env y) ,env)]
      [`(lambda (,x) ,body) `(,(lambda (arg env) (value-of-dynamic-env body (extend-env env x arg))) ,env)]
      [`(,rator ,rand)
        (match (value-of-dynamic-env rator env)
          [`(,clos ,env)
            (match (value-of-dynamic-env rand env)
              [`(,a ,env) (clos a env)])])])))

(define value-of-dynamic
  (lambda (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(quote ,v) v]
      [`(sub1 ,n)
        (sub1 (value-of-dynamic n env))]
      [`(* ,m ,n)
        (* (value-of-dynamic m env) (value-of-dynamic n env))]
      [`(zero? ,n)
        (zero? (value-of-dynamic n env))]
      [`(null? ,e)
        (null? (value-of-dynamic e env))]
      [`(car ,e)
        (car (value-of-dynamic e env))]
      [`(cdr ,e)
        (cdr (value-of-dynamic e env))]
      [`(cons ,a ,d)
        (cons (value-of-dynamic a env) (value-of-dynamic d env))]
      [`(if ,test ,then ,else)
        (if (value-of-dynamic test env)
            (value-of-dynamic then env)
            (value-of-dynamic else env))]
      [`(let ([,x ,y]) ,body)
        (let ([v (value-of-dynamic y env)])
          (value-of-dynamic body (extend-env env x v)))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (closure-ds x body env)]
      [`(,rator ,rand)
        (match (value-of-dynamic rator env)
          [`((,x . ,body) . ,env-old) (value-of-dynamic body (extend-env env x (value-of-dynamic rand env)))])])))

#|Brainteasers|#
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

(define closure-fn-ri
  (lambda (x body env extend-env-ri value-of-ri-rec)
    (lambda (arg)
      ((value-of-ri-rec (extend-env-ri env x arg)) body))))

(define apply-closure-fn-ri
  (lambda (exp val extend-env-ri value-of-ri-rec)
    (exp val)))

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

(define closure-ds-ri
  (lambda (x body env extend-env-ri value-of-ri-rec)
    `((,x . ,body) . ,env)))

(define apply-closure-ds-ri
  (lambda (exp val extend-env-ri value-of-ri-rec)
    (match exp
      [`() (error "empty env")]
      [`((,x . ,body) . ,env) ((value-of-ri-rec (extend-env-ri env x val)) body)])))


(define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (letrec ([value-of-ri-rec
              (lambda (env)
                (lambda (e)
                  (match e
                    [`,n
                     #:when (natural? n)
                     n]
                    [`,b
                     #:when (boolean? b)
                     b]
                    [`(zero? ,n)
                     (zero? ((value-of-ri-rec env) n))]
                    [`(sub1 ,n)
                     (sub1 ((value-of-ri-rec env) n))]
                    [`(* ,x ,y)
                     (* ((value-of-ri-rec env) x) ((value-of-ri-rec env) y))]
                    [`(if ,c ,b1 ,b2)
                     (if ((value-of-ri-rec env) c) ((value-of-ri-rec env) b1) ((value-of-ri-rec env) b2))]
                    [`(let ([,x ,y]) ,body)
                     #:when (symbol? x)
                     (let ([v ((value-of-ri-rec env) y)])
                       ((value-of-ri-rec (extend-env env x v)) body))]
                    [`,y
                     #:when (symbol? y)
                     (apply-env env y)]
                    [`(lambda (,x) ,body)
                     #:when (symbol? x)
                     (closure x body env extend-env value-of-ri-rec)]
                    [`(,rator ,rand)
                     (apply-closure ((value-of-ri-rec env) rator) ((value-of-ri-rec env) rand) extend-env value-of-ri-rec)])))])
               (value-of-ri-rec empty-env))))
#;(trace value-of-ri)

#|Just Dessert|#
(define subst
  (lambda (sym x body)
    (if (eqv? x body) sym body)))

(define alpha-all
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) x]
      [`(lambda (,x) ,body)
       (let ((g (gensym (symbol->string x))))
         `(lambda (,g) ,(subst g x (alpha-all body))))]
      [`(,rator ,rand)
       `(,(alpha-all rator) ,(alpha-all rand))])))

(define reducer
  (lambda (under before)
    (lambda (str)
      (letrec
        ((reducer (lambda (exp)
	   (match exp
	     [`,x #:when (symbol? x) x]
	     [`(lambda (,x) ,body)
	      `(lambda (,x) ,((under reducer) body))]
	     [`(,rator ,rand)
	      (match (reducer rator)
		[`(lambda (,x) ,body)
		 (str (subst ((before reducer) rand) x (alpha-all body)))]
		[`,else `(,else ,((before reducer) rand))])]))))
        reducer))))

(define yes (lambda (f) (lambda (x) (f x))))
(define no (lambda (f) (lambda (x) x)))

(define by-value (reducer no yes))

(define applicative (reducer yes yes))
(define head-spine (reducer yes no))
(define by-name (reducer no no))

(define ao-nf
  (letrec ((str (lambda (exp) ((applicative str) exp))))
    str))

(define bv-wnf
  (letrec ((str (lambda (exp) ((by-value str) exp))))
    str))

(define he-hnf
  (letrec ((str (lambda (exp) ((head-spine str) exp))))
    str))

(define bn-whnf
  (letrec ((str (lambda (exp) ((by-name str) exp))))
    str))

(define no-nf (applicative bn-whnf))
(define ha-nf (applicative bv-wnf))
(define hn-nf (applicative he-hnf))
