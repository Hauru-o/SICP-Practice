#lang racket

(define (square n)
  (* n n))

(define (cons-stream x y)
  (cons x y))

(define (head s)
  (car s))

(define (tail s)
  (cdr s))

(define (empty-stream? s)
  (null? s))

(define THE-EMPTY-STREAM '())

(define (map-stream proc s)
  (if (empty-stream? s)
      THE-EMPTY-STREAM
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter pred s)
  (cond
    ((empty-stream? s) THE-EMPTY-STREAM)
    ((pred (head s))
     (cons-stream (head s)
                  (filter pred
                          (tail s))))
    (else (filter pred (tail s)))))

(define (accumulate combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate combiner
                            init-val
                            (tail s)))))

; (define (enumerate-tree tree)
;   (if (leaf-node? tree)
;       (cons-stream tree
;                    THE-EMPTY-STREAM)
;       (append-streams
;        (enumerate-tree
;         (left-branch tree))
;        (enumerate-tree
;         (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream
       (head s1)
       (append-streams (tail s1)
                       s2))))

(define (enum-interval low high)
  (if (> low high)
      THE-EMPTY-STREAM
      (cons-stream
       low
       (enum-interval (+ low 1) high))))

; (define (sum-odd-squares tree)
;   (accumulate
;    +
;    0
;    (map
;     square
;     (filter odd?
;             (enumerate-tree tree)))))

(define (odd-fibs n)
  (accumulate
   cons
   '()
   (filter
    odd?
    (map fib (enum-interval 1 n)))))