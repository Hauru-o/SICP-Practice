#lang racket

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
  (lambda (L evn)
    (cond ((eq? L '()) '())
          (else
           (cons (eval (car L) env)
                 (evlist (cdr l) env))))))

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
             (else (error TMA))))
      ((eq? vals '()) (error TFA))
      (else
       (cons (cons (car vars)
                   (car vals))
             (pair-up (cdr vars)
                      (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error UBV))
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