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
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cadr l))
      (make-product (car l) (make-product-list (cdr l)))))

(define *precedence-table*
  '((maxop . 10000)
    (minop . -10000)
    (+ . 0)
    (* . 1)))

(define (operator? x)
  (and (symbol? x) ; 只处理符号
       (let loop ((op-pair *precedence-table*))
         (cond ((null? op-pair) #f)
               ((eq? x (caar op-pair)) #t)
               (else (loop (cdr op-pair)))))))


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
           (cdar op-pair))
          (else
           (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (accumulate combiner null-value expr)
  (if (null? expr)
      null-value
      (combiner (car expr) (accumulate combiner null-value (cdr expr)))))


(define (smallest-op expr)
  (accumulate (lambda (a b)
                (if (operator? b)
                    (min-precedence a b)
                    a))
              'maxop
              expr))

(define (sum? x)
  (eq? '+ (smallest-op x)))

(define (prefix sym list)
  (if (or (null? list) (eq? sym (car list)))
      '()
      (cons (car list) (prefix sym (cdr list)))))

(define (singleton? lst)
  (and (pair? lst) (null? (cdr lst))))


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

(define (product? x)
  (eq? '* (smallest-op x)))

(define (multiplier expr)
  (let ((m (prefix '* expr)))
    (if (singleton? m)
        (car m)
        m)))

(define (multiplicand expr)
  (let ((m (cdr (memq '* expr))))
    (if (singleton? m)
        (car m)
        m)))

(define (make-product m1 m2)
  (cond ((=number? m1 1)  m2)
        ((=number? m2 1)  m1)
        ((or (=number? m1 0) (=number? m2 0))  0)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

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


(deriv '(x + 3 * (x + y + 2)) 'x)