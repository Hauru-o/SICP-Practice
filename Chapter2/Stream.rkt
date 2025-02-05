#lang racket

(define (square n)
  (* n n))

; (define (cons-stream x y)
;   (cons x y))

; (define (head s)
;   (car s))

; (define (tail s)
;   (cdr s))

(define (memo-proc proc)
  (let ((already-run? #f) (result null))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? #t)
            result)
          result))))

(define (delay-stream exp)
  (memo-proc (lambda () exp)))

(define (force-stream proc)
  (proc))

(define (cons-stream x y)
  (cons x (delay-stream y)))

(define (head s)
  (car s))

(define (tail s)
  (force-stream (cdr s)))

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

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (odd-fibs n)
  (accumulate
   cons
   '()
   (filter
    odd?
    (map fib (enum-interval 1 n)))))

(define (flatten st-of-st)
  (accumulate append-streams THE-EMPTY-STREAM st-of-st))

(define (faltmap f s)
  (flatten (map f s)))

(define (prime? n)
  (define (divides? x y)
    (= (remainder y x) 0))  ; 检查y是否能被x整除

  (define (check-divisors d)
    (cond ((>= (* d d) n) #t)  ; 如果d的平方大于等于n，则n是质数
          ((divides? d n) #f)  ; 如果d能整除n，则n不是质数
          (else (check-divisors (+ d 1)))))  ; 否则递归检查下一个d

  (and (> n 1) (check-divisors 2)))  ; 判断n是否大于1，并开始检查2到sqrt(n)的所有除数


(define (prime-sum-pairs n)
  (map
   (lambda (p)
     (list (car p)
           (cadr p)
           (+ (car p) (cadr p))))
   (filter
    (lambda (p)
      (prime? (+ (car p) (cadr p))))
    (faltmap
     (lambda (i)
       (map
        (lambda (j) (list i j))
        (enum-interval 1 (- i 1))))
     (enum-interval 1 n)))))

(prime-sum-pairs 5)

; collect 语法糖版
; (define (prime-sum-pairs-collect n)
;   (collect
;    (list i j (+ i j))
;    ((i (enum-interval 1 n))
;     (j (enum-interval 1 (- i 1))))
;    (prime? (+ i j))))

; 八皇后问题
; (define (queens size)
;   (define (fill-cols k)
;     (if
;      (= k 0)
;      (singleton empty-board)
;      (collect
;       (adjoin-position try-row
;                        k
;                        rest-queens)
;       ((rest-queens (fill-cols (- k 1)))
;        (try-row (enum-interval 1 size)))
;       (safe? try-row k rest-queens))))
;   (fill-cols size))

(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (- n 1) (tail s))))

(define (print-stream s)
  (cond ((empty-stream? s) "done")
        (else (print (head s))
              (print-stream (tail s)))))

(define (integers-from n)
  (cons-stream n
               (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (divisible? a b)
  (= (remainder a b) 0))

(define (sieve s)
  (cons-stream
   (head s)
   (sieve (filter
           (lambda (x)
             (not
              (divisible? x (head s))))
           (tail s)))))

(define primes (sieve (integers-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (cons-stream
          (+ (head s1) (head s2))
          (add-streams (tail s1) (tail s2))))))

(define (scale-stream c s)
  (map-stream (lambda (x) (* x c)) s))

(define integers-increase
  (cons-stream 1
               (add-streams integers-increase ones)))

(define (integral s initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream dt s)
                  int)))
  int)

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams fibs (tail fibs)))))