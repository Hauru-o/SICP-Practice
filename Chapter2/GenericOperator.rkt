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
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (angle-rectangular z)
  (atan (cdr z) (car z)))

(define (make-polar r a)
  (attach-type 'polar (cons r a)))

(define (real-part-polar z)
  (* (car z) (cos (cdr z))))

(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))


(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

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

(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))
        (proc (contents obj))
        (error "undefine op"))))

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
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (-rat x y)
  (make-rat
   (- (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (/rat x y)
  (make-rat
   (* (numer x) (denom y))
   (* (denom x) (numer y))))

(put 'rational 'add +rat)
(put 'rational 'sub -rat)
(put 'rational 'mul *rat)
(put 'rational 'div /rat)

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

(define (add x y)
  (operate-2 'add x y))

(define (sub x y)
  (operate-2 'sub x y))

(define (mul x y)
  (operate-2 'mul x y))

(define (div x y)
  (operate-2 'div x y))

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