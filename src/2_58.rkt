#lang sicp

(define (=number? x n)
  (and (number? x) (= x n)))

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0)
         a2)
        ((=number? a2 0)
         a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1)
         m2)
        ((=number? m2 1)
         m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else
         (list m1 '* m2))))

(define (accumulate combiner null-value expr)
  (if (null? expr)
      null-value
      (combiner (car expr)
                (accumulate combiner null-value (cdr expr)))))

(define (sum? x)
  (and (pair? x)
       (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x)
       (eq? (cadr x) '*)))

(define (addend s)
  (car s))
(define (augend s)
  (caddr s))

(define (prefix sym list)
  (if (or (null? list) (eq? sym (car list)))
      '()
      (cons (car list) (prefix sym (cdr list)))))

(define (multiplier p)
  (car p))
(define (multiplicand p)
  (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknow expression type -- DERIV" exp))))

(make-product 'x 'y)
(make-sum 'x 'y)
(deriv '((x * y) * (x + 3)) 'x)