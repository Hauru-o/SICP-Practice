#lang sicp

(define (=number? x n)
  (and (number? x) (= x n)))

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cdr l)))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (make-sum-list (list a1 a2)))))

;(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cadr l))
      (make-product (car l) (make-product-list (cdr l)))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (make-product-list (list m1 m2)))))

(define (accumulate combiner null-value expr)
  (if (null? expr)
      null-value
      (combiner (car expr)
                (accumulate combiner null-value (cdr expr)))))

(define (smallest-op expr)
  (accumulate (lambda (a b)
                (if (operator? b)
                    (min-precedence a b)
                    a))
              'maxop
              expr))

(define *precedence-table*
  '((maxop . 10000)
    (minop . -10000)
    (+ . 0)
    (* . 1)))

(define (operator? x)
  (define (loop op-pair)
    (cond ((null? op-pair) #f)
          ((eq? x (caar op-pair)) #t)
          (else (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (min-precedence a b)
  (if (precedence<? a b)
      a
      b))

(define (precedence<? a b)
  (< (precedence a) (precedence b)))

(define (precedence op)
  (define (loop op-pair)
    (cond ((null? op-pair)
           (error "Operator not defined -- PRECEDENCE:" op))
          ((eq? op (caar op-pair))
           (cadr op-pair))
          (else
           (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (sum? x)
  (eq? '+ (smallest-op x)))

(define (product? x)
  (eq? '* (smallest-op x)))

; (define (addend s) (cadr s))
; (define (augend s)
;   (let ((a (cddr s)))
;     (if (= (length a) 1)
;         (car a)
;         (make-sum-list a))))
(define (addend expr)
  (let ((a (prefix '+ expr)))
    (if (singleton? a)
        (car a)
        a)))
(define (augend expr)
  (let ((a (cdr (memq '+ expr))))
    (if (singleton? a)
        (car a)
        a)))

(define (prefix sym list)
  (if (or (null? list) (eq? sym (car list)))
      '()
      (cons (car list) (prefix sym (cdr list)))))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (let ((m (cddr p)))
    (if (= (length m) 1)
        (car m)
        (make-product-list m))))

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
