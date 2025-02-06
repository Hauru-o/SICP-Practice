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