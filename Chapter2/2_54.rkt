#lang sicp

(define (equal? x y)
    (cond ((and (not (pair? x)) (not (pair? y)))
            (eq? x y))
            ((and (pair? x) (pair? y))
            (and (equal? (car x) (car y))  (equal? (cdr x) (cdr y))))
            (else false)))

(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6))

(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6))