#lang racket

(define user-initial-environment (make-base-namespace))

(define (atom? x)
  (not (or (pair? x) (null? x))))

;; Expressions

(define (compound? exp) (pair?   exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom?   exp))

;; Rules

(define (pattern  rule) (car  rule))
(define (skeleton rule) (cadr rule))

;; Patterns

(define (arbitrary-constant?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?c) false))

(define (arbitrary-expression?  pattern)
  (if (pair? pattern) (eq? (car pattern) '? ) false))

(define (arbitrary-variable?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?v) false))

(define (variable-name pattern) (cadr pattern))

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         (if (atom? exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dictionary pat exp dict))
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
           (cdr exp)
           (match (car pat)
             (car exp)
             dict)))))

(define (skeleton-evaluation?    skeleton)
  (if (pair? skeleton) (eq? (car skeleton) ':) false))

(define (eval-exp evaluation) (cadr evaluation))

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (map (lambda (v)
              (lookup v dict))
            (cdr form)))))

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict
                 (match (pattern (car rules))
                   exp
                   (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                 (instantiate
                     (skeleton (car rules))
                   dict))))))
    (scan the-rules))
  simplify-exp)

(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v)
        var
        (cadr v))))