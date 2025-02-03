#lang racket

(define (make-storage)
  (define store '())

  (define (put key1 key2 value)
    (set! store
          (cons
           (cons
            (list key1 key2)
            value)
           store)))

  (define (get key1 key2)
    (define (find-key lst)
      (cond ((null? lst) #f)
            ((equal? (car (car lst))
                     (list key1 key2))
             (cdr (car lst)))
            (else (find-key (cdr lst)))))
    (find-key store))

  (cons put get))

(define storage (make-storage))

(define put (car storage))
(define get (cdr storage))

(define (square a)
  (* a a))

(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))

(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))
        (proc (contents obj))
        (error "undefine op"))))

(define (operate-2 op arg1 arg2)
  (if
   (eq? (type arg1) (type arg2))
   (let ((proc (get (type arg1) op)))
     (if (not (null? proc))
         (proc (contents arg1)
               (contents arg2))
         (error
          "Op undefined on type")))
   (error "Args not same type")))

(define (ADD x y)
  (operate-2 'add x y))
(define (SUB x y)
  (operate-2 'sub x y))
(define (MUL x y)
  (operate-2 'mul x y))
(define (DIV x y)
  (operate-2 'div x y))

(define (rectangular? z)
  (eq? (type z) 'rectangular))

(define (polar? z)
  (eq? (type z) 'polar))

(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define (real-part-rectangular z)
  (car z))

(define (imag-part-rectangular z)
  (cdr z))

(define (magnitude-rectangular z)
  (sqrt (ADD (square (car z))
             (square (cdr z)))))

(define (angle-rectangular z)
  (atan (cdr z) (car z)))

(define (make-polar r a)
  (attach-type 'polar (cons r a)))

(define (real-part-polar z)
  (MUL (car z) (cos (cdr z))))

(define (imag-part-polar z)
  (MUL (car z) (sin (cdr z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))


(define (+c z1 z2)
  (make-rectangular
   (ADD (real-part z1) (real-part z2))
   (ADD (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular
   (SUB (real-part z1) (real-part z2))
   (SUB (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
   (MUL (magnitude z1) (magnitude z2))
   (ADD (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
   (DIV (magnitude z1) (magnitude z2))
   (SUB (angle z1) (angle z2))))

(put 'rectangular 'real-part
     real-part-rectangular)
(put 'rectangular 'imag-part
     imag-part-rectangular)
(put 'rectangular 'magnitude
     magnitude-rectangular)
(put 'rectangular 'angle
     angle-rectangular)
(put 'polar 'real-part
     real-part-polar)
(put 'polar 'imag-part
     imag-part-polar)
(put 'polar 'magnitude
     magnitude-polar)
(put 'polar 'angle
     angle-polar)

(define (real-part z)
  (operate 'real-part z))

(define (imag-part z)
  (operate 'imag-part z))

(define (magnitude z)
  (operate 'magnitude z))

(define (angle z)
  (operate 'angle z))

(define (make-rat n d)
  (attach-type 'rational (cons n d)))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (+rat x y)
  (make-rat
   (ADD (MUL (numer x) (denom y))
        (MUL (numer y) (denom x)))
   (MUL (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (MUL (numer x) (numer y))
   (MUL (denom x) (denom y))))

(define (-rat x y)
  (make-rat
   (SUB (MUL (numer x) (denom y))
        (MUL (numer y) (denom x)))
   (MUL (denom x) (denom y))))

(define (/rat x y)
  (make-rat
   (MUL (numer x) (denom y))
   (MUL (denom x) (numer y))))

(put 'rational 'add +rat)
(put 'rational 'sub -rat)
(put 'rational 'mul *rat)
(put 'rational 'div /rat)


(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

(define (-complex z1 z2)
  (make-complex (-c z1 z2)))

(define (*complex z1 z2)
  (make-complex (*c z1 z2)))

(define (/complex z1 z2)
  (make-complex (/c z1 z2)))

(put 'complex 'add +complex)
(put 'complex 'sub -complex)
(put 'complex 'mul *complex)
(put 'complex 'div /complex)

(define (make-number n)
  (attach-type 'number n))

(define (+number x y)
  (make-number (+ x y)))

(define (-number x y)
  (make-number (- x y)))

(define (*number x y)
  (make-number (* x y)))

(define (/number x y)
  (make-number (/ x y)))

(put 'number 'add +number)
(put 'number 'sub -number)
(put 'number 'mul *number)
(put 'number 'div /number)

(define (make-polynomial var term-list)
  (attach-type 'polynomial
               (cons var term-list)))

(define (same-var? var1 var2)
  (eq? var1 var2))

(define (var p)
  (car p))

(define (term-list p)
  (cdr p))

(define (empty-termlist? lst)
  (or (not lst) (null? lst)))

(define (first-term lst)
  (car lst))

(define (rest-terms lst)
  (cdr lst))

(define (order term)
  (car term))

(define (coeff term)
  (cdr term))

(define (make-term o c)
  (cons o c))

(define (adjoin-term term termlist)
  (cons term termlist))

(define (+terms l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (else
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (cond
             ((> (order t1) (order t2))
              (adjoin-term
               t1
               (+terms (rest-terms l1) l2)))
             ((< (order t1) (order t2))
              (adjoin-term
               t2
               (+terms l1 (rest-terms l2))))
             (else
              (adjoin-term
               (make-term (order t1)
                          (ADD (coeff t1)
                               (coeff t2)))
               (+terms (rest-terms l1)
                       (rest-terms l2)))))))))

(define (+poly p1 p2)
  (if (same-var? (var p1) (var p2))
      (make-polynomial
       (var p1)
       (+terms (term-list p1)
               (term-list p2)))
      (error "Polys not in same var")))

(put 'polynomial 'add +poly)