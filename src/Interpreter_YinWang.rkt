#lang racket

(let ([x 1]
      [y 2])
  (+ x y))

(define (tree-sum tree)
  (cond
    ((eq? tree '()) 0)
    ((number? tree) tree)
    (else
     (+ (tree-sum (car tree))
        (tree-sum (cdr tree))))))

(define tree-sum-match
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,e1 ,e2)
       (let ([v1 (tree-sum e1)]
             [v2 (tree-sum e2)])
         (+ v1 v2))])))

(tree-sum '((1 2) (3 4)))
(tree-sum-match '((1 2) (3 4)))

(define calc
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,op ,e1 ,e2)
       (let ([v1 (calc e1)]
             [v2 (calc e2)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

(calc '(* (+ 1 2) (+ 3 4)))

(define env0 '())

(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) #f]
        [else
         (cdr p)]))))

(struct Closure (f env))

(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
           [(not v)
            (error "undefined variable" x)]
           [else v]))]
      [(? number? x) x]
      [`(lambda (,x) ,e)
       (Closure exp env)]
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

(define r2
  (lambda (exp)
    (interp exp env0)))