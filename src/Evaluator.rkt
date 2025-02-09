#lang racket

;; Default eval func
; (define eval
;   (lambda (exp env)
;     (cond ((number? exp) exp)
;           ((symbol? exp) (lookup exp env))
;           ((eq? (car exp) 'quote) (cadr exp))
;           ((eq? (car exp) 'lambda)
;            (list 'closure (car exp) env))
;           ((eq? (car exp) 'cond)
;            (evcond (cdr exp) env))
;           (else
;            (apply (eval (car exp) env)
;                   (evlist (cdr exp) env))))))

;; Default apply func
; (define apply
;   (lambda (proc args)
;     (cond ((primitive? proc)
;            (apply-primop proc args))
;           ((eq? (car proc) 'closure)    ; (closure ((x) (+ x y)) <env>)
;            (eval (cadadr proc)
;                  (bind (caadr proc)
;                        args
;                        (caddr proc))))
;           (else error))))

;; eavl func with dynamic bind
; (define eval
;   (lambda (exp env)
;     (cond ((number? exp) exp)
;           ((symbol? exp) (lookup exp env))
;           ((eq? (car exp) 'quote) (cadr exp))
;           ((eq? (car exp) 'lambda) exp)
;           ((eq? (car exp) 'cond)
;            (evcond (cdr exp) env))
;           (else
;            (apply (eval (car exp) env)
;                   (evlist (cdr exp) env)
;                   env)))))

; (define (apply-primop proc args)
;   ('()))

;; apply func with dynamic bind
; (define apply
;   (lambda (proc args env)
;     (cond ((primitive? proc)
;            (apply-primop proc args))
;           ((eq? (car proc) 'lambda)    ;; proc == (lambda bvrs body)
;            (eval (caddr proc)
;                  (bind (cadr proc)
;                        args
;                        env)))
;           (else error))))


;; eval func with args name feature
(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cadr exp))
          ((eq? (car exp) 'lambda)
           (list 'closure (car exp) env))
          ((eq? (car exp) 'cond)
           (evcond (cdr exp) env))
          (else
           (apply (eval (car exp) env)
                  (evlist (cdr exp) env))))))

;; apply func with args name feature
(define apply
  (lambda (proc args)
    (cond ((primitive? proc)
           (apply-primop proc args))
          ((eq? (car proc) 'closure)    ; (closure ((x) (+ x y)) <env>)
           (eval (cadadr proc)
                 (bind (caadr proc)
                       args
                       (caddr proc))))
          (else error))))


(define evlist
  (lambda (L env)
    (cond ((eq? L '()) '())
          (else
           (cons (eval (car L) env)
                 (evlist (cdr L) env))))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (eval (caar clauses) env))
           (evcond (cdr clauses) env))
          (else
           (eval (cadar clauses) env)))))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals)
          env)))

(define pair-up
  (lambda (vars vals)
    (cond
      ((eq? vars '())
       (cond ((eq? vals '()) '())
             (else (error 'TMA))))
      ((symbol? vars)
       (cons (cons vars vals) '()))
      ((eq? vals '()) (error 'TFA))
      (else
       (cons (cons (car vars)
                   (car vals))
             (pair-up (cdr vars)
                      (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error 'UBV))
          (else
           ((lambda (vcell)
              (cond ((eq? vcell '())
                     (lookup sym
                             (cdr env)))
                    (else (cdr vcell))))
            (assq sym (car env)))))))

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist))
           (car alist))
          (else
           (assq sym (cdr alist))))))


; ---------- (Y F) = (F (Y F)) ------------

(define expt
  (lambda (x n)
    (cond ((= n 0) 1)
          (else
           (* x (expt x (- n 1)))))))

(define F
  (lambda (g)
    (lambda (x n)
      (cond ((= n 0) 1)
            (else
             (* x
                (g x (- n 1))))))))

; ((lambda (x) (x x)) (lambda (x) (x x)))

(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

; ----------- Dynamic Bind ---------------

(define 1+
  (lambda (x)
    (+ x 1)))

(define sum
  (lambda (term a next b)
    (cond ((> a b) 0)
          (else
           (+ (term a)
              (sum term
                   (next a)
                   next
                   b))))))

(define product
  (lambda (term a next b)
    (cond ((> a b) 0)
          (else
           (* (term a)
              (product term
                       (next a)
                       next
                       b))))))

; (define sum-powers
;   (lambda (a b n)
;     (sum (lambda (x) (expt x n))
;          a
;          1+
;          b)))

; (define product-powers
;   (lambda (a b n)
;     (product (lambda (x) (expt x n))
;              a
;              1+
;              b)))

;; Use Dynamic Bind
; (define sum-powers
;   (lambda (a b n)
;     (sum nth-power a 1+ b)))

; (define product-powers
;   (lambda (a b n)
;     (product nth-power a 1+ b)))

; (define nth-power
;   (lambda (x)
;     (expt x n)))

;; Not use dynamic bind
(define pgen
  (lambda (n)
    (lambda (x )(expt x n))))

(define sum-powers
  (lambda (a b n)
    (sum (pgen n) a 1+ b)))

(define product-powers
  (lambda (a b n)
    (product (pgen n) a 1+ b)))

(define (unless p c a)
  (cond ((not p) c)
        (else a)))

(unless (= 1 0) 2 (/ 1 0))  ;; this will be error

(cond ((not (= 1 0)) 2)     ;; this will run well
      (else (/ 1 0)))

(define (unless p (name c) (name a))
  (cond ((not p) c)
        (else a)))